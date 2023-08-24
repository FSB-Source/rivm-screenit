package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.main.dao.cervix.CervixHuisartsDao;
import nl.rivm.screenit.main.service.cervix.CervixBulkHuisartsenService;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixBulkUpload;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.CodeGenerator;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.apache.commons.lang.StringUtils;
import org.hibernate.NonUniqueResultException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVReader;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixBulkHuisartsenServiceImpl implements CervixBulkHuisartsenService
{
	private final ExecutorService executorService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private CervixHuisartsDao cervixHuisartsDao;

	public CervixBulkHuisartsenServiceImpl()
	{
		this.executorService = Executors.newSingleThreadExecutor();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkBulkHuisartsen(CervixBulkUpload cervixBulkUpload)
	{
		executorService.submit(new OpenHibernate5SessionInThread()
		{
			@Override
			protected void runInternal()
			{
				verwerkBestand(cervixBulkUpload);
			}
		});
	}

	private void verwerkBestand(CervixBulkUpload bulkUpload)
	{
		try
		{
			UploadDocument document = bulkUpload.getDocument();
			File file = uploadDocumentService.load(document);
			try (FileInputStream fileInputStream = new FileInputStream(file))
			{
				logging(LogGebeurtenis.BULK_HUISARTSEN_VERWERKING_GESTART, null);

				char character = getMostUsefullReader(file);
				try (CSVReader reader = new CSVReader(new InputStreamReader(fileInputStream), character))
				{
					int regel = 0;
					for (String[] line : reader.readAll())
					{
						regel++;
						String informatie = "Bulk huisartsen(" + bulkUpload.getId() + ") - Regel " + regel + " - ";
						try
						{
							LOG.info(informatie + "Bezig met verwerking.");
							boolean isRegelGoedVerwerkt = verwerkRegel(line, informatie);
							if (isRegelGoedVerwerkt)
							{
								LOG.info(informatie + "Regel verwerkt.");
							}
						}
						catch (Exception e)
						{
							LOG.error(informatie + "Regel overgeslagen vanwege onbekende fout: " + e.getMessage());
							melding(informatie + "Regel overgeslagen vanwege onbekende fout.");
							continue;
						}
					}
				}
				logging(LogGebeurtenis.BULK_HUISARTSEN_VERWERKING_AFGEROND, null);
			}
		}
		catch (FileNotFoundException e)
		{
			LOG.error("Bestand niet gevonden", e);
		}
		catch (IOException e)
		{
			LOG.error("Probleem met het bestand", e);
		}
		catch (Exception e)
		{
			LOG.error("Er is iets misgegaan met het bestand inlezen.", e);
		}
	}

	private boolean verwerkRegel(String[] row, String informatie)
	{
		String agbCode = row[3];
		if (StringUtils.isAlpha(agbCode) && !StringUtils.isBlank(agbCode))
		{
			LOG.info(informatie + "Headers van file ontdekt.");
			return false;
		}

		String zorgmail = row[0];
		String aanhef = row[4];
		String achternaam = row[5];
		String voorletter = row[6];
		String tussenvoegsel = row[7];
		String straat = row[12];
		String postcode = row[14].replaceAll(" ", "");
		String huisnummer = row[13].replaceAll("\\D+", "");
		String huisnummertoevoeging = row[13].replaceAll("\\d", "");
		String plaats = StringUtils.lowerCase(row[15]);
		String gemeente = StringUtils.lowerCase(row[16]);
		String locatieNaam = row[11];
		String telefoon = row[18];

		if (StringUtils.isEmpty(agbCode) || StringUtils.isEmpty(achternaam) || StringUtils.isEmpty(straat) || StringUtils.isEmpty(postcode) || StringUtils.isEmpty(huisnummer)
			|| StringUtils.isEmpty(plaats))
		{
			melding(informatie + "Huisarts overgeslagen vanwege niet volledige gegevens.");
			return false;
		}
		agbCode = StringUtils.leftPad(agbCode, 8, '0');
		Woonplaats woonplaats = null;
		try
		{
			woonplaats = cervixHuisartsDao.getWoonplaats(plaats);
		}
		catch (NonUniqueResultException e)
		{

			if (gemeente != null)
			{
				woonplaats = cervixHuisartsDao.getWoonplaats(plaats, gemeente);
			}
			else
			{
				melding(informatie + "Huisarts overgeslagen vanwege dubbele woonplaats gevonden, handmatige verwerking vereist.");
				return false;
			}
		}
		if (woonplaats == null)
		{
			melding(informatie + "Huisarts overgeslagen vanwege het ontbreken van een woonplaats: " + plaats + ".");
			return false;
		}
		if (woonplaats.getGemeente() == null)
		{
			melding(informatie + "Huisarts overgeslagen vanwege het ontbreken van de koppeling tussen woonplaats(" + woonplaats.getNaam() + ") en een gemeente.");
		}
		if (woonplaats.getGemeente().getScreeningOrganisatie() == null)
		{
			melding(informatie + "Huisarts overgeslagen vanwege het ontbreken van de koppeling tussen de gemeente(" + woonplaats.getGemeente().getNaam()
				+ ") en screeningsorganisatie.");
			return false;
		}
		CervixHuisarts arts = cervixHuisartsDao.getHuisarts(agbCode);
		if (arts != null)
		{
			melding(informatie + "Huisarts overgeslagen vanwege dat deze huisarts al bestaat.");
			return false;
		}
		if (arts == null)
		{
			arts = new CervixHuisarts();
			arts.setAanmeldStatus(CervixHuisartsAanmeldStatus.AANGEMAAKT);
			arts.setAgbcode(agbCode);
			arts.setOrganisatieMedewerkers(new ArrayList<>());
			arts.setAanmeldStatus(CervixHuisartsAanmeldStatus.REGISTRATIE_KLAARGEZET);
			arts.setMutatiedatum(currentDateSupplier.getDate());
			arts.setActief(true);
			if (StringUtils.isEmpty(locatieNaam))
			{
				arts.setNaam(locatieNaam);
			}
			else
			{
				arts.setNaam("Praktijk van " + achternaam);
			}
			arts.setTelefoon(telefoon);

			InstellingGebruiker instellingGebruiker = new InstellingGebruiker();
			arts.getOrganisatieMedewerkers().add(instellingGebruiker);
			Gebruiker medewerker = new Gebruiker();
			medewerker.setAanhef(getAanhef(aanhef));
			medewerker.setInlogMethode(InlogMethode.GEBRUIKERSNAAM_WACHTWOORD);
			medewerker.setTussenvoegsel(tussenvoegsel);
			medewerker.setAchternaam(achternaam);
			medewerker.setVoorletters(voorletter);
			medewerker.setActief(true);
			medewerker.setOrganisatieMedewerkers(new ArrayList<>());
			medewerker.getOrganisatieMedewerkers().add(instellingGebruiker);
			medewerker.setGebruikersnaam("ua-" + agbCode);
			instellingGebruiker.setMedewerker(medewerker);
			instellingGebruiker.setOrganisatie(arts);
			instellingGebruiker.setActief(true);

			String codeB = CodeGenerator.genereerCode(3, 3);
			medewerker.setDatumWachtwoordAanvraag(currentDateSupplier.getDate());
			medewerker.setWachtwoordChangeCode(codeB);

			CervixHuisartsAdres adres = new CervixHuisartsAdres();
			adres.setStraat(straat);
			adres.setPostcode(postcode);
			adres.setHuisnummer(Integer.valueOf(huisnummer));
			adres.setHuisnummerToevoeging(huisnummertoevoeging);
			adres.setWoonplaats(woonplaats);
			arts.setPostadres(adres);
			hibernateService.saveOrUpdateAll(medewerker, arts, instellingGebruiker, adres);

			sendBriefNaarHuisarts(arts);
			synchroniseerHuisarts(arts);

		}
		return true;
	}

	private Aanhef getAanhef(String gender)
	{
		if (gender.equals("F"))
		{
			return Aanhef.MEVR;
		}
		else if (gender.equals("M"))
		{
			return Aanhef.DHR;
		}
		return null;
	}

	private void sendBriefNaarHuisarts(CervixHuisarts huisarts)
	{
		Date date = currentDateSupplier.getDate();
		Woonplaats woonplaats = huisarts.getPostadres().getWoonplaats();
		CervixRegioBrief cervixRegioBrief = briefService.maakRegioBrief(woonplaats.getGemeente().getScreeningOrganisatie(), BriefType.REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS,
			date, huisarts);
		cervixRegioBrief.setHuisarts(huisarts);
		hibernateService.saveOrUpdateAll(cervixRegioBrief);
	}

	private void synchroniseerHuisarts(CervixHuisarts huisarts)
	{
		HuisartsDto dto = CervixHuisartsToDtoUtil.getHuisartsDto(huisarts);
		huisartsenportaalSyncService.sendJmsBericht(dto);
	}

	private void synchroniseerLocatie(CervixHuisartsLocatie locatie)
	{
		LocatieDto dto = CervixHuisartsToDtoUtil.getLocatieDto(locatie);
		huisartsenportaalSyncService.sendJmsBericht(dto);
	}

	private void melding(String melding)
	{
		LOG.info(melding);
		logging(LogGebeurtenis.BULK_HUISARTSEN_VERWERKING_MELDING, melding);
	}

	private void logging(LogGebeurtenis gebeurtenis, String melding)
	{
		LogEvent logEvent = new LogEvent();
		logEvent.setMelding(melding);
		logService.logGebeurtenis(gebeurtenis, logEvent, Bevolkingsonderzoek.CERVIX);
	}

	private Character getMostUsefullReader(File file) throws Exception
	{
		CSVReader testReader = null;
		String[] huidigeLine = null;
		try
		{
			testReader = new CSVReader(new FileReader(file), ',');
			huidigeLine = testReader.readNext();
			testReader.close();
			if (huidigeLine != null && Arrays.asList(huidigeLine).size() > 2)
			{
				return ',';
			}
			else
			{
				huidigeLine = null;
				testReader = new CSVReader(new FileReader(file), ';');
				huidigeLine = testReader.readNext();
				testReader.close();
				if (huidigeLine != null && Arrays.asList(huidigeLine).size() > 2)
				{
					return ';';
				}
				else
				{
					testReader = null;
					throw new IllegalStateException("File die is aangeleverd voldoet niet aan het juiste formaat");
				}
			}
		}
		catch (Exception e)
		{
			if (testReader != null)
			{
				try
				{

					testReader.close();
				}
				catch (IOException e1)
				{
					LOG.error("Van het projectbestand kan de reader niet worden gesloten", e);
					throw e;
				}
			}
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public CervixBulkUpload saveExcelBestand(File file, String contentType, String filename, InstellingGebruiker gebruiker) throws IOException
	{
		UploadDocument document = new UploadDocument();
		document.setNaam(filename);
		document.setActief(true);
		document.setContentType(contentType);
		document.setFile(file);
		uploadDocumentService.saveOrUpdate(document, FileStoreLocation.CERVIX_BULK_HUISARTSEN);

		CervixBulkUpload upload = new CervixBulkUpload();
		upload.setGebruiker(gebruiker);
		upload.setDocument(document);
		upload.setUploadDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(upload);
		return upload;
	}
}
