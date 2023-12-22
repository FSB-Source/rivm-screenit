package nl.rivm.screenit.main.service.impl;

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
import java.io.IOException;
import java.io.PushbackInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.BaseUitnodigingDao;
import nl.rivm.screenit.main.service.RetourzendingService;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.RetourredenAfhandeling;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.enums.RetourzendingStatus;
import nl.rivm.screenit.model.colon.enums.RetourzendingWijze;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.logging.RetourzendingLogEvent;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseUitnodigingService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class RetourzendingServiceImpl implements RetourzendingService
{
	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private LogService logService;

	@Autowired
	private BaseUitnodigingService baseUitnodigingsService;

	@Autowired
	private BaseUitnodigingDao uitnodigingDao;

	@Autowired
	private ColonUitnodigingService colonUitnodigingsService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ColonScreeningsrondeService colonScreeningsrondeService;

	@Autowired
	private CervixBaseScreeningrondeService cervixScreeningrondeService;

	@Autowired
	private BaseUitnodigingService uitnodigingService;

	@Autowired
	private CervixFactory cervixFactory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private IFobtService ifobtService;

	@Autowired
	private BaseBriefService briefService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public RetourzendingLogEvent verwerkBestandMetRetourzendingen(InstellingGebruiker ingelogdeGebruiker, String contentType, File file, String fileName) throws IOException
	{
		RetourzendingLogEvent logEvent = new RetourzendingLogEvent();
		UploadDocument uploadDocument = new UploadDocument();
		uploadDocument.setActief(Boolean.TRUE);
		uploadDocument.setContentType(contentType);
		uploadDocument.setFile(file);
		uploadDocument.setNaam(fileName);

		uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.COLON_RETOURZENDING);

		logEvent.setSanddBestand(uploadDocument);

		try (Workbook workbook = WorkbookFactory.create(new PushbackInputStream(new FileInputStream(uploadDocumentService.load(uploadDocument)))))
		{
			Sheet sheet = workbook.getSheetAt(0);
			if (sheet == null)
			{
				throw new IllegalStateException("Geen sheet in het excel gevonden");
			}

			int index = 0;
			Map<String, Integer> columnIndex = new HashMap<>();
			boolean first = true;
			while (sheet.getLastRowNum() >= index)
			{
				Row row = sheet.getRow(index++);

				if (first)
				{
					for (int j = row.getFirstCellNum(); j < row.getLastCellNum(); j++)
					{
						columnIndex.put(row.getCell(j).getStringCellValue(), j);
					}
					first = false;
				}
				else
				{
					String trackId = getCellValue(row, columnIndex.get("TrackID"));
					String retourzendingReden = getCellValue(row, columnIndex.get("Reden"));
					String postcode = getCellValue(row, columnIndex.get("Postcode"));

					Integer huisnummer = null;
					try
					{
						huisnummer = Integer.valueOf(getCellValue(row, columnIndex.get("Huisnummer")));
					}
					catch (NumberFormatException ex)
					{
						LOG.error("Geen geldig getal ingevoerd bij huisnummer.");
					}

					boolean isValidRegel = isValidRegel(logEvent, index, trackId, retourzendingReden, postcode, huisnummer);

					boolean skipRegel = true;
					if (isValidRegel)
					{
						RetourredenAfhandeling retourredenAfhandeling = bepaalAfhandelingVoorRetourzending(retourzendingReden);
						ColonUitnodiging colonUitnodiging = baseUitnodigingsService.getUitnodiging(ColonUitnodiging.class, trackId, postcode, huisnummer);
						if (colonUitnodiging != null && colonUitnodiging.getRetourzendingReden() == null)
						{
							if (isDossierInactiefOfRondeAfgerond(colonUitnodiging))
							{
								logEvent.incrGeenRondeActiefRegels(index);
							}
							else if (isValideColonUitnodiging(colonUitnodiging) != null)
							{
								logEvent.incrGeenValideUitnodiging(index);
							}
							else if (!colonScreeningsrondeService.heeftUitslag(colonUitnodiging, false))
							{
								if (retourredenAfhandeling != null)
								{
									verwerkRetourzending(logEvent, colonUitnodiging, retourredenAfhandeling, RetourzendingWijze.BESTAND);
									skipRegel = false;
								}
							}
						}
						CervixUitnodiging cervixUitnodiging = baseUitnodigingsService.getUitnodiging(CervixUitnodiging.class, trackId, postcode, huisnummer);
						if (cervixUitnodiging != null && cervixUitnodiging.getRetourzendingReden() == null)
						{
							if (isDossierInactiefOfRondeAfgerond(cervixUitnodiging))
							{
								logEvent.incrGeenRondeActiefRegels(index);
							}
							else if (isValideCervixUitnodiging(cervixUitnodiging) != null)
							{
								logEvent.incrGeenValideUitnodiging(index);
							}
							else if (!cervixScreeningrondeService.heeftUitslagOfHeeftGehad(cervixUitnodiging))
							{
								if (retourredenAfhandeling != null)
								{
									verwerkRetourzending(logEvent, cervixUitnodiging, retourredenAfhandeling, RetourzendingWijze.BESTAND);
									skipRegel = false;
								}
							}
						}

						if (skipRegel && heeftAlRetourStatus(colonUitnodiging, cervixUitnodiging))
						{
							logEvent.incrZendingHeeftAlRetourstatusRegels(index);
						}
						if (cervixUitnodiging == null && colonUitnodiging == null)
						{
							logEvent.incrClientNietGevondenRegels(index);
						}
					}
					if (skipRegel)
					{
						logEvent.incrSkippedRegels();
					}
				}
			}
		}
		catch (Exception e)
		{
			logEvent.setLevel(Level.ERROR);
			LOG.error("Fout bij lezen van bestand", e);
		}
		logService.logGebeurtenis(LogGebeurtenis.RETOURZENDINGEN_VERWERKT, logEvent, ingelogdeGebruiker, Bevolkingsonderzoek.COLON);
		return logEvent;
	}

	private boolean heeftAlRetourStatus(ColonUitnodiging colonUitnodiging, CervixUitnodiging cervixUitnodiging)
	{
		return colonUitnodiging != null && colonUitnodiging.getRetourzendingReden() != null
			|| cervixUitnodiging != null && cervixUitnodiging.getRetourzendingReden() != null;
	}

	private boolean isValidRegel(RetourzendingLogEvent logEvent, int index, String trackId, String retourzendingReden, String postcode, Integer huisnummer)
	{
		boolean isValid = true;
		boolean trackIDgevondenColon = uitnodigingDao.uitnodigingExists(ColonUitnodiging.class, trackId);
		boolean trackIDgevondenCervix = uitnodigingDao.uitnodigingExists(CervixUitnodiging.class, trackId);

		if (huisnummer == null || StringUtils.isBlank(postcode))
		{
			logEvent.incrClientNietGevondenRegels(index);
			isValid = false;
		}

		if (!trackIDgevondenCervix && !trackIDgevondenColon)
		{
			logEvent.incrTrackIdNietGevondenRegels(index);
			isValid = false;
		}

		if (StringUtils.isBlank(retourzendingReden))
		{
			logEvent.incrRetourRedenNietGevondenRegels(index);
			isValid = false;
		}
		else
		{
			RetourredenAfhandeling retourredenAfhandeling = bepaalAfhandelingVoorRetourzending(retourzendingReden);
			if (retourredenAfhandeling == null)
			{
				logEvent.incrRetourRedenNietGevondenRegels(index);
				isValid = false;
			}
		}
		return isValid;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public <U extends InpakbareUitnodiging<S>, S extends ScreeningRonde<?, ?, ?, ?>> void verwerkRetourzendingHandmatig(InstellingGebruiker ingelogdeGebruiker, U uitnodiging,
		String retourzendingReden)
	{
		RetourzendingLogEvent logEvent = new RetourzendingLogEvent();
		RetourredenAfhandeling retourredenAfhandeling = bepaalAfhandelingVoorRetourzending(retourzendingReden);
		if (retourredenAfhandeling == null)
		{
			throw new IllegalArgumentException("Retourzendingreden: afhandeling is onbekend!");
		}
		verwerkRetourzending(logEvent, uitnodiging, retourredenAfhandeling, RetourzendingWijze.HANDMATIG);

		Bevolkingsonderzoek bvo = uitnodiging instanceof ColonUitnodiging ? Bevolkingsonderzoek.COLON : Bevolkingsonderzoek.CERVIX;
		logService.logGebeurtenis(LogGebeurtenis.RETOURZENDINGEN_VERWERKT, logEvent, ingelogdeGebruiker, uitnodiging.getScreeningRonde().getDossier().getClient(), bvo);
	}

	private <U extends InpakbareUitnodiging<S>, S extends ScreeningRonde<?, ?, ?, ?>> void verwerkRetourzending(RetourzendingLogEvent logEvent, U uitnodiging,
		RetourredenAfhandeling afhandeling, RetourzendingWijze wijze)
	{
		uitnodiging.setRetourOntvangen(currentDateSupplier.getDate());
		uitnodiging.setRetourzendingReden(afhandeling.getRetourReden());
		uitnodiging.setRetourzendingWijze(wijze);
		logEvent.incrRegels(afhandeling.getAfhandeling());
		S screeningRonde = uitnodiging.getScreeningRonde();

		switch (afhandeling.getAfhandeling())
		{
		case NIEUWE_GBA_AANVRAAG:
			if (baseUitnodigingsService.isAdresGewijzigdNaUitnodigingsdatum(uitnodiging))
			{
				uitnodiging.setRetourzendingStatus(RetourzendingStatus.NIEUWE_UITNODIGING_DIRECT_AANGEVRAAGD);
				if (uitnodiging instanceof ColonUitnodiging)
				{
					colonUitnodigingsService.cloneUitnodiging((ColonUitnodiging) uitnodiging, false);
				}
				else if (uitnodiging instanceof CervixUitnodiging)
				{
					cervixFactory.maakZasUitnodiging(screeningRonde.getDossier().getClient(), null, false, false);
				}
			}
			else if (!baseUitnodigingsService.isVerstuurdMetTijdelijkAdres(uitnodiging))
			{
				GbaVraag gbaVraag = clientService.vraagGbaGegevensOpnieuwAan(screeningRonde.getDossier().getClient(), null, RedenGbaVraag.ONJUIST_ADRES);
				String retourzendingMarker = null;
				if (uitnodiging instanceof ColonUitnodiging)
				{
					retourzendingMarker = Constants.COLON_RETOURZENDING_MARKER;
				}
				else if (uitnodiging instanceof CervixUitnodiging)
				{
					retourzendingMarker = Constants.CERVIX_RETOURZENDING_MARKER;
				}
				String aanvullendeInformatie = "|" + Constants.GBA_CHECK_ON_TIJDELIJK_ADRES_NU_ACTUEEL + "|" + Constants.RETOURZENDING_UITNODIGINGS_ID_MARKER + uitnodiging.getId()
					+ "|" + retourzendingMarker + "|";
				gbaVraag.setAanvullendeInformatie(aanvullendeInformatie);
				hibernateService.saveOrUpdate(gbaVraag);
				uitnodiging.setRetourzendingStatus(RetourzendingStatus.NIEUWE_GBA_ADRES_AANGEVRAAGD);
			}
			else
			{
				uitnodiging.setRetourzendingStatus(RetourzendingStatus.TIJDELIJK_ADRES_GEEN_NIEUWE_UITNODIGING_MOGELIJK);
			}
			break;
		case DIRECT_NIEUWE_UITNODIGING:
			uitnodiging.setRetourzendingStatus(RetourzendingStatus.NIEUWE_UITNODIGING_DIRECT_AANGEVRAAGD);
			if (uitnodiging instanceof ColonUitnodiging)
			{
				colonUitnodigingsService.cloneUitnodiging((ColonUitnodiging) uitnodiging, false);
			}
			else if (uitnodiging instanceof CervixUitnodiging)
			{
				cervixFactory.maakZasUitnodiging(screeningRonde.getDossier().getClient(), null, false, false);
			}
			break;

		case GEWEIGERD:
			if (uitnodiging instanceof ColonUitnodiging)
			{
				ColonScreeningRonde ronde = (ColonScreeningRonde) screeningRonde;
				if (!colonScreeningsrondeService.isRondeStatusBuitenDoelgroep(ronde))
				{
					briefService.maakBvoBrief(ronde, BriefType.COLON_ZENDING_GEWEIGERD);
				}
			}
			else if (uitnodiging instanceof CervixUitnodiging)
			{
				briefService.maakBvoBrief((CervixScreeningRonde) screeningRonde, BriefType.CERVIX_ZENDING_GEWEIGERD);
			}
			break;
		default:
			throw new IllegalStateException();
		}
		if (uitnodiging instanceof ColonUitnodiging)
		{
			ifobtService.markeerBuisAlsVerloren((ColonUitnodiging) uitnodiging);
		}

		hibernateService.saveOrUpdate(uitnodiging);
	}

	private RetourredenAfhandeling bepaalAfhandelingVoorRetourzending(String retourReden)
	{
		List<RetourredenAfhandeling> retourredenAfhandelingList = hibernateService.loadAll(RetourredenAfhandeling.class);
		for (RetourredenAfhandeling retourRedenAfhandeling : retourredenAfhandelingList)
		{
			if (retourRedenAfhandeling.getRetourReden().equalsIgnoreCase(retourReden))
			{
				return retourRedenAfhandeling;
			}
		}
		return null;
	}

	@Override
	public <U extends Uitnodiging> boolean isDossierInactiefOfRondeAfgerond(U uitnodiging)
	{
		if (uitnodiging.getScreeningRonde().getStatus().equals(ScreeningRondeStatus.AFGEROND)
			|| uitnodiging.getScreeningRonde().getDossier().getStatus().equals(DossierStatus.INACTIEF))
		{
			return true;
		}
		return false;
	}

	@Override
	public String isValideColonUitnodiging(ColonUitnodiging uitnodiging)
	{
		if (!uitnodigingService.heeftAlEenNieuwereUitnodiging(uitnodiging))
		{
			if (!colonScreeningsrondeService.heeftUitslag(uitnodiging, false))
			{
				IFOBTTest test = uitnodiging.getGekoppeldeTest();
				if (test != null)
				{
					if (test.getStatus() == IFOBTTestStatus.ACTIEF)
					{
						return null;
					}
					else
					{
						return "DK.al.beoordeeld.of.opnieuw.aangevraagd";
					}
				}
				else
				{
					return "DK.geen.test.gekoppeld";
				}
			}
			else
			{
				return "DK.uitslagbrief.reeds.verstuurd";
			}
		}
		else
		{
			return "DK.er.is.al.een.nieuwe.uitnodiging";
		}
	}

	@Override
	public String isValideCervixUitnodiging(CervixUitnodiging uitnodiging)
	{
		if (!cervixScreeningrondeService.heeftUitslagOfHeeftGehad(uitnodiging))
		{
			CervixZas test = CervixMonsterUtil.getZAS(uitnodiging.getMonster());
			if (test != null)
			{
				if (test.getZasStatus() == CervixZasStatus.VERSTUURD && uitnodiging.getGeannuleerdDatum() == null)
				{
					return null;
				}
				else
				{
					return "BMHK.al.beoordeeld.of.opnieuw.aangevraagd";
				}
			}
			else
			{
				return "BMHK.geen.test.gekoppeld";
			}
		}
		else
		{
			return "BMHK.uitslagbrief.reeds.verstuurd";
		}
	}

	private String getCellValue(Row row, int index)
	{
		Cell cell = row.getCell(index);
		String cellValue = null;
		if (cell != null)
		{
			if (cell.getCellType() == CellType.NUMERIC)
			{
				cellValue = "" + (int) cell.getNumericCellValue();
			}
			else
			{
				cellValue = cell.getStringCellValue();
			}
		}
		return cellValue;
	}
}
