package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Huisarts;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.weergave.persoon.NaamWeergaveHelper;

import org.apache.commons.lang.StringUtils;

import com.google.common.base.Strings;

public abstract class NaamUtil
{
	
	public static String getNaamGebruiker(Gebruiker gebruiker)
	{
		if (gebruiker == null)
		{
			return null;
		}

		StringBuilder ingelogdeGebruikerNaam = new StringBuilder();
		if (StringUtils.isNotBlank(gebruiker.getVoorletters()))
		{
			ingelogdeGebruikerNaam.append(NaamWeergaveHelper.standaardiseerVoorletters(gebruiker.getVoorletters()));
			ingelogdeGebruikerNaam.append(" ");
		}
		if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
		{
			ingelogdeGebruikerNaam.append(gebruiker.getTussenvoegsel());
			ingelogdeGebruikerNaam.append(" ");

		}
		ingelogdeGebruikerNaam.append(gebruiker.getAchternaam());
		return ingelogdeGebruikerNaam.toString();
	}

	public static List<String> getNamenInstellingGebruikers(List<InstellingGebruiker> instellingGebruikers)
	{
		return instellingGebruikers.stream().map(gebruiker -> gebruiker.getMedewerker().getNaamVolledig()).collect(Collectors.toList());
	}

	public static String getTussenvoegselEnAchternaam(Gebruiker gebruiker)
	{
		if (gebruiker == null)
		{
			return null;
		}

		StringBuilder ingelogdeGebruikerNaam = new StringBuilder();
		if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
		{
			ingelogdeGebruikerNaam.append(gebruiker.getTussenvoegsel());
			ingelogdeGebruikerNaam.append(" ");

		}
		ingelogdeGebruikerNaam.append(gebruiker.getAchternaam());
		return ingelogdeGebruikerNaam.toString();
	}

	public static String getTussenvoegselEnEigenAchternaam(GbaPersoon persoon)
	{
		if (persoon == null)
		{
			return null;
		}

		StringBuilder naam = new StringBuilder();
		if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			naam.append(persoon.getTussenvoegsel());
			naam.append(" ");

		}
		naam.append(persoon.getAchternaam());
		return naam.toString();
	}

	public static String getNaamClientMetBsn(Client client, boolean withClosure)
	{
		String naamClient = titelVoorlettersTussenvoegselEnAanspreekAchternaam(client);
		if (Strings.isNullOrEmpty(naamClient))
		{
			return null;
		}
		StringBuilder naam = new StringBuilder();
		naam.append(naamClient);
		GbaPersoon persoon = client.getPersoon();
		if (!Strings.isNullOrEmpty(persoon.getBsn()))
		{
			naam.append(" (Bsn: ");
			naam.append(ng01BsnSafeCheck(persoon.getBsn()));
			if (!withClosure)
			{
				naam.append(" ");
			}
		}
		if (!Strings.isNullOrEmpty(persoon.getBsn()) && withClosure)
		{
			naam.append(")");
		}
		return naam.toString();
	}

	public static String ng01BsnSafeCheck(String bsn)
	{
		if (bsn.length() == 12)
		{
			return bsn.substring(3, bsn.length());
		}
		return bsn;
	}

	public static String getNaamClientMetBsnMetGeboortedatum(Client client)
	{
		StringBuilder naam = new StringBuilder();
		String naamClient = getNaamClientMetBsn(client, false);
		if (Strings.isNullOrEmpty(naamClient))
		{
			return null;
		}
		GbaPersoon persoon = client.getPersoon();
		naam.append(naamClient);
		if (persoon.getGeboortedatum() != null)
		{
			if (Strings.isNullOrEmpty(persoon.getBsn()))
			{
				naam.append(" (");
			}
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			naam.append("Geboortedatum: ");
			naam.append(format.format(persoon.getGeboortedatum()));
			naam.append(")");
		}
		else if (!Strings.isNullOrEmpty(persoon.getBsn()))
		{
			naam.append(")");
		}
		return naam.toString();
	}

	public static String titelVoorlettersTussenvoegselEnAanspreekAchternaam(Client client)
	{
		if (client == null)
		{
			return null;
		}

		StringBuilder naam = new StringBuilder();

		GbaPersoon persoon = client.getPersoon();
		if (!Strings.isNullOrEmpty(persoon.getTitel()))
		{
			naam.append(persoon.getTitel());
			naam.append(" ");
		}

		String voorletters = getVoorlettersClient(client);
		naam.append(voorletters);
		if (!Strings.isNullOrEmpty(voorletters))
		{
			naam.append(" ");
		}

		naam.append(getAanspreekTussenvoegselEnAchternaam(client));

		return naam.toString();
	}

	public static String getAanspreekTussenvoegselEnAchternaam(Client client)
	{
		if (client == null)
		{
			return null;
		}

		NaamGebruik naamGebruik = client.getPersoon().getNaamGebruik();
		if (NaamGebruik.EIGEN.equals(naamGebruik) || NaamGebruik.EIGEN_PARTNER.equals(naamGebruik))
		{
			return client.getPersoon().getNaamEigenPartner();
		}
		else if (naamGebruik == null || NaamGebruik.PARTNER.equals(naamGebruik) || NaamGebruik.PARTNER_EIGEN.equals(naamGebruik))
		{
			return client.getPersoon().getNaamPartnerEigen();
		}
		return null;
	}

	public static String getAchternaamVoorlettersTussenvoegsel(Client client)
	{
		if (client == null)
		{
			return null;
		}
		String volledigeNaam = "";

		Persoon persoon = client.getPersoon();
		NaamGebruik naamGebruik = persoon.getNaamGebruik();
		if (NaamGebruik.EIGEN.equals(naamGebruik) || NaamGebruik.EIGEN_PARTNER.equals(naamGebruik))
		{
			if (StringUtils.isNotBlank(persoon.getAchternaam()))
			{
				volledigeNaam += persoon.getAchternaam();
			}
			if (NaamGebruik.EIGEN_PARTNER.equals(naamGebruik))
			{
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
				{
					volledigeNaam += " - ";
				}
				if (StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()))
				{
					volledigeNaam += persoon.getPartnerTussenvoegsel() + " ";
				}
				if (StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
				{
					volledigeNaam += persoon.getPartnerAchternaam();
				}
			}
		}
		else if (naamGebruik == null || NaamGebruik.PARTNER.equals(naamGebruik) || NaamGebruik.PARTNER_EIGEN.equals(naamGebruik))
		{
			if (StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
			{
				volledigeNaam += persoon.getPartnerAchternaam();
			}
			if (naamGebruik == null || NaamGebruik.PARTNER_EIGEN.equals(naamGebruik))
			{
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getAchternaam()))
				{
					volledigeNaam += " - ";
				}
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getTussenvoegsel()))
				{
					volledigeNaam += persoon.getTussenvoegsel() + " ";
				}
				if (StringUtils.isNotBlank(persoon.getAchternaam()))
				{
					volledigeNaam += persoon.getAchternaam();
				}
			}
		}

		volledigeNaam += ", " + getVoorlettersClient(client);
		if (((NaamGebruik.EIGEN.equals(naamGebruik) || NaamGebruik.EIGEN_PARTNER.equals(naamGebruik))
			|| (NaamGebruik.PARTNER_EIGEN.equals(naamGebruik) && StringUtils.isBlank(persoon.getPartnerTussenvoegsel()) && StringUtils.isBlank(persoon.getPartnerAchternaam())))
			&& StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			volledigeNaam += " " + persoon.getTussenvoegsel();
		}
		else if ((NaamGebruik.PARTNER.equals(naamGebruik) || NaamGebruik.PARTNER_EIGEN.equals(naamGebruik)) && StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()))
		{
			volledigeNaam += " " + persoon.getPartnerTussenvoegsel();
		}
		else if (naamGebruik == null)
		{
			if (StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()) && StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
			{
				volledigeNaam += " " + persoon.getPartnerTussenvoegsel();
			}
			else if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
			{
				volledigeNaam += " " + persoon.getTussenvoegsel();
			}
		}

		return volledigeNaam;
	}

	public static String getVoorlettersClient(Client client)
	{
		if (client == null)
		{
			return null;
		}

		StringBuilder voorletters = new StringBuilder();

		if (!Strings.isNullOrEmpty(client.getPersoon().getVoornaam()))
		{
			String[] voornamen = client.getPersoon().getVoornaam().split(" ");
			for (String voornaam : voornamen)
			{
				if (!Strings.isNullOrEmpty(voornaam))
				{
					voorletters.append(voornaam.toUpperCase().charAt(0));

					if (voornaam.toUpperCase().startsWith("IJ"))
					{
						voorletters.append(voornaam.toUpperCase().charAt(1));
					}

					voorletters.append(".");
				}
			}
		}
		return voorletters.toString();
	}

	public static String getNaamHuisarts(Huisarts huisarts)
	{
		StringBuilder naam = new StringBuilder();
		if (huisarts == null)
		{
			return naam.toString();
		}

		if (StringUtils.isNotBlank(huisarts.getVoorletters()))
		{
			naam.append(NaamWeergaveHelper.standaardiseerVoorletters(huisarts.getVoorletters()));
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(huisarts.getTussenvoegels()))
		{
			naam.append(huisarts.getTussenvoegels());
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(huisarts.getAchternaam()))
		{
			naam.append(huisarts.getAchternaam());
		}

		return naam.toString();
	}

	public static String getNaamHuisarts(CervixHuisarts cervixHuisarts)
	{
		StringBuilder naam = new StringBuilder();
		if (cervixHuisarts == null)
		{
			return naam.toString();
		}
		Gebruiker medewerker = cervixHuisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
		if (StringUtils.isNotBlank(medewerker.getVoorletters()))
		{
			naam.append(NaamWeergaveHelper.standaardiseerVoorletters(medewerker.getVoorletters()));
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
		{
			naam.append(medewerker.getTussenvoegsel());
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(medewerker.getAchternaam()))
		{
			naam.append(medewerker.getAchternaam());
		}

		return naam.toString();
	}

	public static String getNaamOnbekendeHuisarts(OnbekendeHuisarts huisarts)
	{
		StringBuilder naam = new StringBuilder();
		if (huisarts == null)
		{
			return naam.toString();
		}

		if (StringUtils.isNotBlank(huisarts.getHuisartsNaam()))
		{
			naam.append(huisarts.getHuisartsNaam());
		}

		if (StringUtils.isNotBlank(huisarts.getPraktijkNaam()))
		{
			if (naam.length() > 0)
			{
				naam.append(" / ");
			}
			naam.append(huisarts.getPraktijkNaam());
		}
		return naam.toString();
	}

	public static String getGeboorteTussenvoegselEnAchternaam(Persoon persoon)
	{
		String result = StringUtils.isNotBlank(persoon.getTussenvoegsel()) ? persoon.getTussenvoegsel() + " " : "";
		if (StringUtils.isNotBlank(persoon.getAchternaam()))
		{
			result += persoon.getAchternaam();
		}
		return result;
	}
}
