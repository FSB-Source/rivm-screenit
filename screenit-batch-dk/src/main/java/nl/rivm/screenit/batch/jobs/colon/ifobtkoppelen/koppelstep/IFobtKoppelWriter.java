package nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.koppelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.NonUniqueResultException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.IfobtKoppelenConstants;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.IfobtKoppelingBeeindigdLogEvent;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import generated.KOPPELDATA.VERZONDENUITNODIGING;
import generated.KOPPELDATA.VERZONDENUITNODIGING.MATCHINGFIELDS.MATCHINGFIELD;

@Component
@Slf4j
public class IFobtKoppelWriter implements ItemWriter<VERZONDENUITNODIGING>
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private IFobtService iFobtService;

	private StepExecution stepExecution;

	private IfobtKoppelingBeeindigdLogEvent logEvent;

	@Override
	public void write(List<? extends VERZONDENUITNODIGING> items) throws Exception
	{
		logEvent = (IfobtKoppelingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext().get(IfobtKoppelenConstants.RAPPORTAGEKEYIFOBTKOPPELEN);

		var verzendDatumFormat = new SimpleDateFormat("dd-MM-yyyy");
		for (VERZONDENUITNODIGING verzondenUitnodiging : items)
		{
			String ifobtBarcodeGold = null;
			String ifobtBarcodeExtra = null;
			String trackTraceId = null;
			try
			{
				Map<String, Long> parameters = new HashMap<String, Long>();
				parameters.put("uitnodigingsId", verzondenUitnodiging.getID());
				ColonUitnodiging uitnodiging = hibernateService.getUniqueByParameters(ColonUitnodiging.class, parameters);

				ColonOnderzoeksVariant onderzoeksVariant = null;
				if (uitnodiging != null)
				{
					onderzoeksVariant = uitnodiging.getOnderzoeksVariant();
				}
				boolean barcodeGoldVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.GOLD);
				boolean barcodeExtraVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.STUDIE);

				ifobtBarcodeGold = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.COLON_KOPPEL_BARCODE_GOLD, barcodeGoldVerplicht);
				ifobtBarcodeExtra = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA, barcodeExtraVerplicht);
				trackTraceId = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_TRACK_ID, false);

				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				uitnodiging.setTrackTraceId(trackTraceId);
				Date datumVerstuurd = verzendDatumFormat.parse(getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_DATUM_VERZENDING, true));

				var screeningRonde = uitnodiging.getScreeningRonde();

				koppelTestIndienMogelijk(ifobtBarcodeGold, IFOBTType.GOLD, uitnodiging, datumVerstuurd, screeningRonde);
				koppelTestIndienMogelijk(ifobtBarcodeExtra, IFOBTType.STUDIE, uitnodiging, datumVerstuurd, screeningRonde);

				hibernateService.saveOrUpdate(uitnodiging);
				hibernateService.saveOrUpdate(screeningRonde);

				logEvent.setAantalIfobtenVerwerkt(logEvent.getAantalIfobtenVerwerkt() + 1);
			}
			catch (ParseException | NonUniqueResultException e)
			{
				LOG.error("Fout bij verwerken van koppel data regel ", e);
				throwError(String.format(KoppelConstants.COLON_ONBEKENDE_FOUT, verzondenUitnodiging.getID(), StringUtils.defaultIfBlank(ifobtBarcodeGold, "<geen>"),
					StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), StringUtils.defaultIfBlank(trackTraceId, "<geen>"), e.getMessage()));
			}
		}
	}

	private void koppelTestIndienMogelijk(String ifobtBarcode, IFOBTType ifobtType, ColonUitnodiging uitnodiging, Date datumVerstuurd, ColonScreeningRonde screeningRonde)
	{
		if (StringUtils.isNotBlank(ifobtBarcode))
		{
			IFOBTTest test;
			if (ifobtType.equals(IFOBTType.GOLD))
			{
				test = uitnodiging.getGekoppeldeTest();
			}
			else
			{
				test = uitnodiging.getGekoppeldeExtraTest();
			}

			if (test != null && !test.getBarcode().equals(ifobtBarcode))
			{
				test = null;
			}
			if (test == null)
			{
				test = new IFOBTTest();
				test.setType(ifobtType);
				test.setBarcode(ifobtBarcode);
			}
			test.setColonScreeningRonde(screeningRonde);
			test.setColonUitnodiging(uitnodiging);
			test.setDatumVerstuurd(datumVerstuurd);

			if (test.getStatus() == null)
			{
				if (uitnodiging.equals(screeningRonde.getLaatsteUitnodiging()))
				{
					iFobtService.setStatus(test, IFOBTTestStatus.ACTIEF);
					if (ifobtType.equals(IFOBTType.GOLD))
					{
						screeningRonde.setLaatsteIFOBTTest(test);
					}
					else
					{
						screeningRonde.setLaatsteIFOBTTestExtra(test);
					}
				}
				else
				{
					iFobtService.setStatus(test, IFOBTTestStatus.VERLOREN);
				}
				if (ifobtType.equals(IFOBTType.GOLD))
				{
					screeningRonde.setLaatsteIFOBTTestExtra(null);
				}
			}

			if (ifobtType.equals(IFOBTType.GOLD))
			{
				uitnodiging.setGekoppeldeTest(test);
			}
			else
			{
				uitnodiging.setGekoppeldeExtraTest(test);
			}
			hibernateService.saveOrUpdate(test);
		}
	}

	private void throwError(String melding)
	{
		LOG.error(melding);
		logEvent.setLevel(Level.ERROR);
		logEvent.setMelding(melding);
		throw new IllegalStateException(melding);
	}

	private String getMatchingFieldValue(VERZONDENUITNODIGING verzondenUitnodiging, String matchingFieldName, boolean required)
	{
		for (MATCHINGFIELD matchingField : verzondenUitnodiging.getMATCHINGFIELDS().getMATCHINGFIELD())
		{
			if (matchingField.getNAME().equalsIgnoreCase(matchingFieldName))
			{
				return matchingField.getVALUE();
			}
		}

		if (required)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bericht had niet alle gegevens beschikbaar - MatchingField: " + matchingFieldName);
			throw new IllegalStateException("MatchingField not found: " + matchingFieldName);
		}
		return null;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}

	public void setIfobtService(IFobtService iFobtService)
	{
		this.iFobtService = iFobtService;
	}
}
