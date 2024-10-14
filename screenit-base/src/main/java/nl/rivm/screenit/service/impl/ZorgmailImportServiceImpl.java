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

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ZorgmailImportMapping;
import nl.rivm.screenit.service.ZorgmailImportService;
import nl.rivm.screenit.service.ZorgmailImportVoortgang;
import nl.topicuszorg.gba.model.Land;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import au.com.bytecode.opencsv.CSVReader;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ZorgmailImportServiceImpl implements ZorgmailImportService
{

	private static final Logger LOG = LoggerFactory.getLogger(ZorgmailImportServiceImpl.class);

	private static final String HUISARTS_ROLE = "Huisarts";

	private static final String HUISARTSPRAKTIJK_ROLE = "Huisartsenpraktijk";

	@Autowired
	private EnovationHuisartsService enovationHuisartsService;

	@Autowired
	private LogService logService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Override
	public void importHandmatigAdresboek(InputStream csvStream, Boolean ediAdresOverschrijven)
	{
		LOG.info("Zorgmail adresboek import is gestart. (EDI-adres overschrijven: " + ediAdresOverschrijven + ")");
		CSVReader reader = new CSVReader(new InputStreamReader(csvStream), ',');
		ZorgmailImportVoortgang voortgang = verwerkCsv(reader, ediAdresOverschrijven);
		enovationHuisartsService.verwijderdeHuisartsenOntkoppelen();

		logService.logGebeurtenis(LogGebeurtenis.HUISARTS_IMPORT_AFGEROND, null,
			"Importeren van Huisartsen Adresboek is afgerond. Zorgmail adresboek import is voltooid. Aantal nieuwe Huisartsen: " + voortgang.getNieuweHuisartsen()
				+ ", aantal geupdate Huisartsen: " + voortgang.getGeupdateHuisartsen() + ", aantal geinactiveerde Huisartsen: " + voortgang.getGeinactiveerdeHuisartsen()
				+ ". EDI-adres overschrijven: " + ediAdresOverschrijven + ".",
			Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA);

		LOG.info("Zorgmail adresboek import is voltooid. Aantal nieuwe Huisartsen: " + voortgang.getNieuweHuisartsen() + ", aantal geupdate Huisartsen: "
			+ voortgang.getGeupdateHuisartsen() + ", aantal geinactiveerde Huisartsen: " + voortgang.getGeinactiveerdeHuisartsen());
	}

	private ZorgmailImportVoortgang verwerkCsv(CSVReader reader, Boolean ediAdresOverschrijven)
	{
		ZorgmailImportVoortgang voortgang = new ZorgmailImportVoortgang();

		try
		{
			int lineNumber = 0;
			ZorgmailImportMapping mapping = null;
			for (String[] line : reader.readAll())
			{
				lineNumber++;
				try
				{
					if (lineNumber == 1)
					{
						mapping = maakMapping(line);
					}
					else
					{
						verwerkLine(line, lineNumber, mapping, voortgang, ediAdresOverschrijven);
					}
				}
				catch (IllegalStateException | ParseException | IndexOutOfBoundsException e)
				{
					LOG.warn("Regel #" + lineNumber + " is niet verwerkt", e);
				}
			}
		}
		catch (IOException e)
		{
			LOG.error("Fout bij verwerken zorgmail bestand", e);
		}
		return voortgang;
	}

	@Override
	public void verwerkLine(String[] line, int lineNumber, ZorgmailImportMapping mapping, ZorgmailImportVoortgang voortgang, Boolean ediAdresOverschrijven) throws ParseException
	{
		String klantnummer = StringUtils.defaultIfBlank(line[mapping.getKlantnummer()], "");
		if (StringUtils.isNotBlank(klantnummer))
		{
			EnovationHuisarts bestaandeHuisarts = enovationHuisartsService.getHuisartsByKlantnummer(klantnummer);
			boolean verwijderd = isVerwijderd(line, mapping);
			boolean huisartsOfHuisartsPraktijk = isHuisartsOfHuisartsPraktijk(line, mapping);
			if (bestaandeHuisarts != null && !bestaandeHuisarts.isVerwijderd() && !verwijderd && !huisartsOfHuisartsPraktijk)
			{

				verwijderd = true;
			}
			if (verwijderd || huisartsOfHuisartsPraktijk)
			{
				voortgang.incrTotaalAantalRijen();
				voortgang.addKlantnummer(klantnummer);

				SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
				Date gewijzigd = format.parse(line[mapping.getGewijzigd()]);

				if (gewijzigd != null)
				{
					if (bestaandeHuisarts == null)
					{
						EnovationHuisarts nieuweHuisarts = new EnovationHuisarts();
						boolean verwerkt = verwerkLineTotHuisarts(line, lineNumber, mapping, nieuweHuisarts, voortgang, ediAdresOverschrijven);

						if (verwerkt && !verwijderd)
						{
							voortgang.incrNieuweHuisartsen();
						}
						else if (verwerkt && verwijderd)
						{
							voortgang.incrGeinactiveerdeHuisartsen();
						}
					}
					else if (bestaandeHuisarts != null && bestaandeHuisarts.getGewijzigd().before(gewijzigd))
					{
						boolean verwerkt = verwerkLineTotHuisarts(line, lineNumber, mapping, bestaandeHuisarts, voortgang, ediAdresOverschrijven);

						if (verwerkt && !verwijderd)
						{
							voortgang.incrGeupdateHuisartsen();
						}
						else if (verwerkt && verwijderd)
						{
							voortgang.incrGeinactiveerdeHuisartsen();
						}
					}
				}
				else
				{
					LOG.warn("Datum kon niet worden gevonden of geformatteerd, regel overgeslagen. #" + lineNumber);
				}
			}
			else
			{
				LOG.warn("#" + lineNumber + " overgeslagen. Of geen role of anders dan 'Huisarts' of 'Huisartsenpraktijk'");
			}
		}
		else
		{
			LOG.warn("Klantnummer kon niet worden ingelezen of NULL, regel overgelagen. #" + lineNumber);
		}
	}

	private boolean verwerkLineTotHuisarts(String[] line, int lineNumber, ZorgmailImportMapping mapping, EnovationHuisarts huisarts, ZorgmailImportVoortgang voortgang,
		Boolean ediAdresOverschrijven)
	{
		try
		{
			SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
			boolean verwijderd = isVerwijderd(line, mapping) || !isHuisartsOfHuisartsPraktijk(line, mapping); 
			if (!verwijderd)
			{
				if (mapping.getGeslacht() > -1)
				{
					huisarts.setGeslacht(verwerkGeslacht(line[mapping.getGeslacht()], isHuisarts(line, mapping)));
				}
				if (mapping.getAchternaam() > -1)
				{
					huisarts.setAchternaam(line[mapping.getAchternaam()]);
				}
				if (mapping.getEdiAdres() > -1)
				{
					String ediParameter = simplePreferenceService.getString(PreferenceKey.EDIFACTADRES.name());
					if (ediAdresOverschrijven && StringUtils.isNotEmpty(ediParameter))
					{
						huisarts.setEdiadres(ediParameter);
					}
					else
					{
						huisarts.setEdiadres(line[mapping.getEdiAdres()]);
					}
					huisarts.setOorspronkelijkEdiadres(line[mapping.getEdiAdres()]);
				}
				if (mapping.getPraktijknaam() > -1)
				{
					huisarts.setPraktijknaam(line[mapping.getPraktijknaam()]);
				}
				if (mapping.getTussenvoegsels() > -1)
				{
					huisarts.setTussenvoegels(line[mapping.getTussenvoegsels()]);
				}
				if (mapping.getInitialen() > -1)
				{
					huisarts.setVoorletters(line[mapping.getInitialen()]);
				}
				if (mapping.getKlantnummer() > -1)
				{
					huisarts.setKlantnummer(line[mapping.getKlantnummer()]);
				}
				if (mapping.getTelefoonnummer() > -1)
				{
					huisarts.setTelefoonnummer(line[mapping.getTelefoonnummer()]);
				}
				if (mapping.getWeergavenaam() > -1)
				{
					huisarts.setWeergavenaam(line[mapping.getWeergavenaam()]);
				}
				if (mapping.getHuisartsagb() > -1)
				{
					huisarts.setHuisartsAgb(verwerkAgb(line[mapping.getHuisartsagb()]));
				}
				if (mapping.getPraktijkagb() > -1)
				{
					huisarts.setPraktijkAgb(verwerkAgb(line[mapping.getPraktijkagb()]));
				}
				huisarts.setAdres(verwerkAdres(line, mapping));
			}
			if (mapping.getGewijzigd() > -1)
			{
				huisarts.setGewijzigd(format.parse(line[mapping.getGewijzigd()]));
			}
			huisarts.setVerwijderd(verwijderd);

			enovationHuisartsService.saveOrUpdate(huisarts);
			LOG.debug("Regel #" + lineNumber + " is verwerkt.");
			return true;

		}
		catch (IllegalStateException | ParseException | IndexOutOfBoundsException e)
		{
			LOG.warn("Regel #" + lineNumber + " is niet verwerkt.", e);
			return false;
		}
	}

	private Adres verwerkAdres(String[] line, ZorgmailImportMapping mapping)
	{
		if (mapping.getStraat() > -1 && mapping.getHuisnummer() > -1 && mapping.getPostcode() > -1 && mapping.getLand() > -1)
		{
			Adres enovationAdres = new Adres();

			String straat = line[mapping.getStraat()];
			String huisnummer = line[mapping.getHuisnummer()];
			String postcode = line[mapping.getPostcode()];
			String plaats = line[mapping.getPlaats()];
			String land = line[mapping.getLand()];

			plaats = StringUtils.lowerCase(plaats);
			plaats = StringUtils.capitalize(plaats);

			postcode = StringUtils.upperCase(postcode);
			postcode = StringUtils.deleteWhitespace(postcode);

			huisnummer = StringUtils.trim(huisnummer);
			String huisnummerToevoeging = null;

			if (!StringUtils.isNumeric(huisnummer))
			{
				char[] huisnummerChars = huisnummer.toCharArray();
				huisnummer = "";
				huisnummerToevoeging = "";
				for (char huisChar : huisnummerChars)
				{
					if (StringUtils.isBlank(huisnummerToevoeging) && CharUtils.isAsciiNumeric(huisChar))
					{
						huisnummer += huisChar;
					}
					else if (CharUtils.isAsciiAlpha(huisChar) || CharUtils.isAsciiNumeric(huisChar))
					{
						if (StringUtils.equals("-", huisnummerToevoeging) && CharUtils.isAsciiAlpha(huisChar))
						{
							huisnummerToevoeging = "";
						}
						huisnummerToevoeging += huisChar;
					}
					else if ('-' == huisChar)
					{
						huisnummerToevoeging += huisChar;
					}
					else if ('/' == huisChar)
					{
						huisnummerToevoeging += huisChar;
					}
					else if ('&' == huisChar)
					{
						huisnummerToevoeging += huisChar;
					}
				}
			}

			Land gevondenLand = null;
			try
			{
				if (StringUtils.isNotBlank(land))
				{
					gevondenLand = Land.valueOf(StringUtils.upperCase(land));
				}
			}
			catch (IllegalArgumentException iae)
			{
				LOG.debug("Land niet gevonden : " + land + ", " + iae.getMessage());
			}

			enovationAdres.setStraat(straat);
			if (StringUtils.isNotBlank(huisnummer) && StringUtils.isNumeric(huisnummer))
			{
				try
				{
					enovationAdres.setHuisnummer(Integer.valueOf(huisnummer));
				}
				catch (NumberFormatException nfe)
				{
					LOG.warn("Huisnummer kon niet worden omgezet: " + huisnummer + ", " + nfe.getMessage());
				}
			}
			enovationAdres.setHuisnummerToevoeging(huisnummerToevoeging);
			enovationAdres.setPlaats(plaats);
			enovationAdres.setPostcode(postcode);
			enovationAdres.setLand(gevondenLand);

			return enovationAdres;
		}
		else
		{
			return null;
		}
	}

	private String verwerkAgb(String agbcode)
	{
		agbcode = StringUtils.deleteWhitespace(agbcode);
		agbcode = StringUtils.remove(agbcode, ".0");

		if (StringUtils.isNotBlank(agbcode) && StringUtils.isNumeric(agbcode) && agbcode.length() >= 7)
		{
			return StringUtils.leftPad(agbcode, 8, '0');
		}
		else
		{
			return null;
		}
	}

	@Override
	public ZorgmailImportMapping maakMapping(String[] row)
	{
		LOG.debug("Begonnen met aanmaken mappings object.");
		ZorgmailImportMapping mapping = new ZorgmailImportMapping();
		int column = 0;
		for (String mappingString : row)
		{
			if (StringUtils.isNotBlank(mappingString))
			{
				if (StringUtils.equalsIgnoreCase("id", mappingString))
				{
					mapping.setKlantnummer(column);
				}
				else if (StringUtils.equalsIgnoreCase("displayName", mappingString))
				{
					mapping.setWeergavenaam(column);
				}
				else if (StringUtils.equalsIgnoreCase("mailAddress", mappingString))
				{
					mapping.setEdiAdres(column);
				}
				else if (StringUtils.equalsIgnoreCase("deleted", mappingString))
				{
					mapping.setGewist(column);
				}
				else if (StringUtils.equalsIgnoreCase("lastModified", mappingString))
				{
					mapping.setGewijzigd(column);
				}
				else if (StringUtils.equalsIgnoreCase("zvAgbCode", mappingString))
				{
					mapping.setHuisartsagb(column);
				}
				else if (StringUtils.equalsIgnoreCase("gender", mappingString))
				{
					mapping.setGeslacht(column);
				}
				else if (StringUtils.equalsIgnoreCase("surname", mappingString))
				{
					mapping.setAchternaam(column);
				}
				else if (StringUtils.equalsIgnoreCase("initials", mappingString))
				{
					mapping.setInitialen(column);
				}
				else if (StringUtils.equalsIgnoreCase("prefix", mappingString))
				{
					mapping.setTussenvoegsels(column);
				}
				else if (StringUtils.equalsIgnoreCase("prAgbCode", mappingString))
				{
					mapping.setPraktijkagb(column);
				}
				else if (StringUtils.equalsIgnoreCase("organization", mappingString))
				{
					mapping.setPraktijknaam(column);
				}
				else if (StringUtils.equalsIgnoreCase("street", mappingString))
				{
					mapping.setStraat(column);
				}
				else if (StringUtils.equalsIgnoreCase("houseNumber", mappingString))
				{
					mapping.setHuisnummer(column);
				}
				else if (StringUtils.equalsIgnoreCase("postalCode", mappingString))
				{
					mapping.setPostcode(column);
				}
				else if (StringUtils.equalsIgnoreCase("locality", mappingString))
				{
					mapping.setPlaats(column);
				}
				else if (StringUtils.equalsIgnoreCase("country", mappingString))
				{
					mapping.setLand(column);
				}
				else if (StringUtils.equalsIgnoreCase("telephoneNumber", mappingString))
				{
					mapping.setTelefoonnummer(column);
				}
				else if (StringUtils.equalsIgnoreCase("role", mappingString))
				{
					mapping.setRole(column);
				}
				else if (StringUtils.equalsIgnoreCase("organisationType", mappingString))
				{
					mapping.setOrganisationType(column);
				}
			}
			column++;
		}
		LOG.debug("Klaar met aanmaken mappings object.");
		return mapping;
	}

	private HuisartsGeslacht verwerkGeslacht(String geslacht, boolean isHuisarts)
	{
		HuisartsGeslacht geslachtEnum = null;

		if (isHuisarts)
		{
			if (StringUtils.equalsIgnoreCase("M", geslacht))
			{
				geslachtEnum = HuisartsGeslacht.MAN;
			}
			else if (StringUtils.equalsIgnoreCase("F", geslacht))
			{
				geslachtEnum = HuisartsGeslacht.VROUW;
			}
			else
			{
				LOG.warn("Onbekende geslachtscode gevonden -> " + geslacht);
				geslachtEnum = HuisartsGeslacht.ONBEKEND;
			}
		}
		else
		{
			geslachtEnum = HuisartsGeslacht.ORGANISATIE;
		}

		return geslachtEnum;
	}

	private boolean isVerwijderd(String[] line, ZorgmailImportMapping mapping)
	{
		return mapping.getGewist() > -1 && StringUtils.equalsIgnoreCase("Y", line[mapping.getGewist()]);
	}

	private boolean isHuisartsOfHuisartsPraktijk(String[] line, ZorgmailImportMapping mapping)
	{
		boolean isHuisarts = isHuisarts(line, mapping);
		boolean isHuisartspraktijk = isHuisartspraktijk(line, mapping);
		return mapping.getRole() > -1 && (isHuisarts || isHuisartspraktijk);
	}

	private boolean isHuisartspraktijk(String[] line, ZorgmailImportMapping mapping)
	{
		return HUISARTSPRAKTIJK_ROLE.equals(line[mapping.getRole()]);
	}

	private boolean isHuisarts(String[] line, ZorgmailImportMapping mapping)
	{
		return HUISARTS_ROLE.equals(line[mapping.getRole()]);
	}
}
