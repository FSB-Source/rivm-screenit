package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ProjectUitslagVerwerkingServiceImpl implements ProjectUitslagVerwerkingService
{
	private static final Logger LOG = LoggerFactory.getLogger(ProjectUitslagVerwerkingServiceImpl.class);

	@Autowired
	private ColonBaseFitService fitService;

	@Autowired
	private ColonStudietestService studietestService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void voorbereidingVoorVerwerking(ProjectUitslagVerwerkingContext context, ProjectBestand uitslagenBestand)
	{
		addUitslagBestandsMelding(uitslagenBestand, null, "Er zijn geen gegevens gevonden");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setBestandStatus(ProjectBestand uitslagenBestand, BestandStatus status)
	{
		setBestandStatus(uitslagenBestand, status, null);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setBestandStatus(ProjectBestand uitslagenBestand, BestandStatus status, String melding)
	{
		uitslagenBestand.setStatus(status);
		uitslagenBestand.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(uitslagenBestand);

		if (melding != null)
		{
			addUitslagBestandsMelding(uitslagenBestand, null, melding);
		}
	}

	private void addUitslagBestandsMelding(ProjectBestand uitslagenBestand, Integer regelnummer, String melding)
	{
		ProjectBestandVerwerking verwerking = uitslagenBestand.getVerwerking();
		ProjectBestandVerwerkingEntry entry = new ProjectBestandVerwerkingEntry();

		entry.setRegelNummer(regelnummer);
		entry.setMelding(melding);
		entry.setVerwerking(verwerking);
		verwerking.getMeldingen().add(entry);
		hibernateService.saveOrUpdateAll(verwerking, entry);
	}

	private void addUitslagBestandsMelding(ProjectBestand uitslagenBestand, ProjectUitslagVerwerkingContext context, String melding, boolean clientIsHeraangemeld)
	{
		Integer regelnummer = context.getRegelnummer();
		String barcode = context.getBarcodeVanHuidigeRegel().trim();
		String uitslagInCSV = context.getUitslagVanHuidigeRegel().trim();

		melding = barcode + ", " + uitslagInCSV + ": " + melding;
		if (clientIsHeraangemeld)
		{
			melding = melding + ". De client was afgemeld voor DK en is heraangemeld.";
		}
		addUitslagBestandsMelding(uitslagenBestand, regelnummer, melding);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkRegel(ProjectUitslagVerwerkingContext context)
	{
		ProjectBestand uitslagenBestand = context.getUitslagenBestand();
		boolean clientIsHeraangemeld = false;

		try
		{
			var barcode = context.getBarcodeVanHuidigeRegel().trim();
			var studietest = fitService.getFit(barcode).orElseThrow(() ->
			{
				var isVerwijderdeBarcode = fitService.isVerwijderdeBarcode(barcode);
				return new ProjectUitslagenUploadException(String.format("Aan deze barcode zijn geen clientgegevens %sgekoppeld", isVerwijderdeBarcode ? "meer " : ""));
			});

			clientIsHeraangemeld = studietestService.studietestHeraanmeldenIndienNodig(studietest);

			studietestService.controleerUitslagenbestandOpFouten(studietest, uitslagenBestand);
			checkEnSetWaardesUitslagenbestand(context, studietest);
			studietestService.verwerkUitslag(studietest);

			ProjectBestandVerwerking verwerking = uitslagenBestand.getVerwerking();
			verwerking.setRegelsVerwerkt(verwerking.getRegelsVerwerkt() + 1);
			hibernateService.saveOrUpdate(verwerking);
		}
		catch (ProjectUitslagenUploadException e)
		{
			addUitslagBestandsMelding(uitslagenBestand, context, e.getMessage(), clientIsHeraangemeld);
			ProjectBestandVerwerking verwerking = uitslagenBestand.getVerwerking();
			verwerking.setRegelsMislukt(verwerking.getRegelsMislukt() + 1);
			hibernateService.saveOrUpdate(verwerking);
			LOG.warn(
				"Probleem opgetreden met verwerken van project uitslag studietest bestand" + uitslagenBestand.getUploadDocument().getNaam() + " op regelnummer "
					+ context.getRegelnummer(),
				e);
		}
	}

	private void checkEnSetWaardesUitslagenbestand(ProjectUitslagVerwerkingContext context, IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		checkEnSetGeinterpreteerdeUitslag(context, studietest);
		checkEnSetAnalysedatum(context, studietest);
		checkEnSetBron(context, studietest);
		checkVervaldatumStudietest(studietest);
	}

	private void checkEnSetGeinterpreteerdeUitslag(ProjectUitslagVerwerkingContext context, IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		String uitslagInCSV = context.getUitslagVanHuidigeRegel().toUpperCase().trim();
		ColonGeinterpreteerdeUitslag geinterpreteerdeUitslag;
		try
		{
			geinterpreteerdeUitslag = ColonGeinterpreteerdeUitslag.valueOf(uitslagInCSV);
		}
		catch (IllegalArgumentException e)
		{
			throw new ProjectUitslagenUploadException("Onbekende waarde voor uitslag: '" + uitslagInCSV + "'");
		}
		studietest.setGeinterpreteerdeUitslag(geinterpreteerdeUitslag);
	}

	private void checkEnSetAnalysedatum(ProjectUitslagVerwerkingContext context, IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		formatter.setLenient(false);
		String analyseDatumInCSV = context.getAnalyseDatum().trim();
		Date analysedatum;
		try
		{
			analysedatum = formatter.parse(analyseDatumInCSV);
		}
		catch (ParseException e)
		{
			LOG.error("Fout bij parsen van value naar datum: " + analyseDatumInCSV);
			throw new ProjectUitslagenUploadException("De analysedatum heeft niet het juiste format, gebruik: dd-mm-yyyy");
		}

		if (analysedatum.after(currentDateSupplier.getDate()))
		{
			throw new ProjectUitslagenUploadException("De analysedatum ligt niet in het verleden");
		}

		studietest.setAnalyseDatum(analysedatum);
	}

	private void checkEnSetBron(ProjectUitslagVerwerkingContext context, IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		String bron = context.getBron().trim();
		if (bron.length() > 255)
		{
			throw new ProjectUitslagenUploadException("De tekst van de bron is te lang. Gebruik maximaal 255 karakters");
		}
		studietest.setInstumentId(bron);
	}

	private void checkVervaldatumStudietest(IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		fitService.checkVervaldatumVerlopen(studietest);
		if (studietest.getStatus().equals(IFOBTTestStatus.VERVALDATUMVERLOPEN))
		{
			throw new ProjectUitslagenUploadException("De houdbaarheidsdatum van de studietest is verlopen");
		}
	}
}
