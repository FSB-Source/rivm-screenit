package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.math.BigDecimal;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.IfobtVerwerkingConstants;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class IFOBTVerwerkingWriter implements ItemWriter<IFOBTUitslag>
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonBaseFitService fitService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private StepExecution stepExecution;

	@Override
	public void write(List<? extends IFOBTUitslag> items)
	{
		var logEvent = (IfobtVerwerkingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext()
			.get(IfobtVerwerkingConstants.RAPPORTAGEKEYVERWERKING);

		var bestanden = logEvent.getRapportage().getBestanden();

		IFOBTBestand bestand = null;
		IfobtVerwerkingRapportageEntry verslagEntry = null;

		for (var ifobtResult : items)
		{
			var ifobtTest = fitService.getFit(ifobtResult.getBarcode()).orElse(null);

			if (bestand == null || !bestand.equals(ifobtResult.getBestand()))
			{
				bestand = ifobtResult.getBestand();
				verslagEntry = null;
			}
			if (verslagEntry == null)
			{
				for (var entry : bestanden)
				{
					if (entry.getIfobtBestandId().equals(bestand.getId()))
					{
						verslagEntry = entry;
						break;
					}
				}
				if (verslagEntry == null)
				{
					verslagEntry = new IfobtVerwerkingRapportageEntry();
					verslagEntry.setBestandsNaam(bestand.getNaamBestand());
					verslagEntry.setIfobtBestandId(bestand.getId());
					verslagEntry.setRapportage(logEvent.getRapportage());
					bestanden.add(verslagEntry);
				}
			}

			if (ifobtTest != null && !IFOBTTestStatus.NIETTEBEOORDELEN.equals(ifobtTest.getStatus()) && ifobtTest.getType().equals(IFOBTType.GOLD))
			{
				var client = ifobtTest.getColonScreeningRonde().getDossier().getClient();

				if (ifobtTest.getUitslag() == null)
				{
					logService.logGebeurtenis(LogGebeurtenis.IFOBT_VERWERKT, client, "barcode: " + ifobtTest.getBarcode(), Bevolkingsonderzoek.COLON);

					zetAnalysegegevensOverNaarFit(bestand, ifobtResult, ifobtTest);

					fitService.uitslagFitOntvangen(ifobtTest);

					verslagEntry.setAantalVerwerkingen(verslagEntry.getAantalVerwerkingen() + 1);
				}
				else
				{
					logService.logGebeurtenis(LogGebeurtenis.IFOBT_UITSLAG_DUBBEL, client, "Voor FIT met barcode " + ifobtTest.getBarcode()
						+ " is al eerder een uitslag verwerkt. Niet nogmaals verwerkt.", Bevolkingsonderzoek.COLON);
				}
			}
			else if (ifobtTest != null && IFOBTTestStatus.NIETTEBEOORDELEN.equals(ifobtTest.getStatus()))
			{
				LOG.warn("Barcode van FIT (id: '{}') hoort bij een onbeoordeelbare FIT.", ifobtTest.getId());
			}
			else
			{
				LOG.warn("Barcode van iFobtResult (id: '{}') is onbekend of hoort niet bij een FIT.", ifobtResult.getId());
			}
			bestand.setAantalVerwerkt(bestand.getAantalVerwerkt() + 1);
			if (bestand.getAantalVerwerkt() >= bestand.getUitslagen().size())
			{
				bestand.setStatus(IFOBTBestandStatus.VERWERKT);
				if (bestand.getAantalVerwerkt() > bestand.getUitslagen().size())
				{
					LOG.warn("Aantal verwerkt is groter uitslagen {} {}", bestand.getAantalVerwerkt(), bestand.getUitslagen().size());
				}
			}

			hibernateService.saveOrUpdate(bestand);
		}
	}

	private void zetAnalysegegevensOverNaarFit(IFOBTBestand bestand, IFOBTUitslag ifobtResult, IFOBTTest ifobtTest)
	{
		ifobtTest.setAnalyseDatum(ifobtResult.getAnalyseDatum());
		ifobtTest.setVerwerkingsDatum(currentDateSupplier.getDate());

		if (correctForInpakcentrumIncident(ifobtTest))
		{
			BigDecimal uitslag = ifobtResult.getUitslag();
			ifobtTest.setUitslag(uitslag);
		}

		ifobtTest.setIfobtLaboratorium(bestand.getLaboratorium());
		ifobtTest.setInstumentId(bestand.getInstumentId());
	}

	private boolean correctForInpakcentrumIncident(IFOBTTest ifobtTest)
	{
		var colonUitnodiging = ifobtTest.getColonUitnodiging();
		if (colonUitnodiging != null)
		{
			String trackTraceId = colonUitnodiging.getTrackTraceId();
			if (trackTraceId != null && (trackTraceId.startsWith("16859/") || trackTraceId.startsWith("17531/")))
			{
				LOG.info("FIT (id: '{}') is aangepast naar status verwijderd (Incident Inpakcentrum)", ifobtTest.getId());
				ifobtTest.setStatus(IFOBTTestStatus.VERWIJDERD);
				ifobtTest.setStatusDatum(currentDateSupplier.getDate());
				return false;
			}
		}
		return true;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
