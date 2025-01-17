package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Huisarts;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
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
		ingelogdeGebruikerNaam.append(getTussenvoegselEnAchternaam(gebruiker));
		return ingelogdeGebruikerNaam.toString();
	}

	public static List<String> getNamenGebruikers(List<Gebruiker> gebruikers)
	{
		return gebruikers.stream().map(Gebruiker::getNaamVolledig).collect(Collectors.toList());
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

		naam.append(voorlettersTussenvoegselEnAanspreekAchternaam(client));
		return naam.toString();
	}

	public static String voorlettersTussenvoegselEnAanspreekAchternaam(Client client)
	{
		if (client == null)
		{
			return null;
		}

		StringBuilder naam = new StringBuilder();
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
		var persoon = client.getPersoon();
		var volledigeNaam = StringUtils.trim(getTussenvoegsel(persoon));
		if (volledigeNaam.length() > 0)
		{
			volledigeNaam += " ";
		}
		volledigeNaam += getAanspreekNaamZonderTussenvoegsel(persoon);
		return volledigeNaam;
	}

	private static String getAanspreekNaamZonderTussenvoegsel(GbaPersoon persoon)
	{
		var volledigeNaam = "";
		var naamGebruik = persoon.getNaamGebruik();
		if (naamGebruik == NaamGebruik.EIGEN || naamGebruik == NaamGebruik.EIGEN_PARTNER)
		{
			if (StringUtils.isNotBlank(persoon.getAchternaam()))
			{
				volledigeNaam += persoon.getAchternaam();
			}
			if (naamGebruik == NaamGebruik.EIGEN_PARTNER)
			{
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
				{
					volledigeNaam += " - ";
				}
				volledigeNaam += getPartnernaam(persoon);
			}
		}
		else if (naamGebruik == null || naamGebruik == NaamGebruik.PARTNER || naamGebruik == NaamGebruik.PARTNER_EIGEN)
		{
			if (StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
			{
				volledigeNaam += persoon.getPartnerAchternaam();
			}
			if (naamGebruik == null || naamGebruik == NaamGebruik.PARTNER_EIGEN)
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
		return volledigeNaam;
	}

	private static String getEigennaam(GbaPersoon persoon)
	{
		var eigennaam = "";
		if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			eigennaam += persoon.getTussenvoegsel() + " ";
		}
		if (StringUtils.isNotBlank(persoon.getAchternaam()))
		{
			eigennaam += persoon.getAchternaam();
		}
		return eigennaam;
	}

	private static String getPartnernaam(GbaPersoon persoon)
	{
		var partnernaam = "";
		if (StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()))
		{
			partnernaam += persoon.getPartnerTussenvoegsel() + " ";
		}
		if (StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
		{
			partnernaam += persoon.getPartnerAchternaam();
		}
		return partnernaam;
	}

	public static String getVolledigeAchternaamVoorlettersTussenvoegsel(Client client)
	{
		if (client == null)
		{
			return null;
		}
		var persoon = client.getPersoon();
		var volledigeNaam = getAanspreekNaamZonderTussenvoegsel(persoon);
		volledigeNaam += ", " + getVoorlettersClient(client);
		volledigeNaam += getTussenvoegsel(persoon);

		return volledigeNaam;
	}

	private static String getTussenvoegsel(GbaPersoon persoon)
	{
		var tussenvoegels = "";
		var naamGebruik = persoon.getNaamGebruik();
		if (((NaamGebruik.EIGEN.equals(naamGebruik) || NaamGebruik.EIGEN_PARTNER.equals(naamGebruik))
			|| (NaamGebruik.PARTNER_EIGEN.equals(naamGebruik) && StringUtils.isBlank(persoon.getPartnerTussenvoegsel()) && StringUtils.isBlank(persoon.getPartnerAchternaam())))
			&& StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			tussenvoegels += " " + persoon.getTussenvoegsel();
		}
		else if ((NaamGebruik.PARTNER.equals(naamGebruik) || NaamGebruik.PARTNER_EIGEN.equals(naamGebruik)) && StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()))
		{
			tussenvoegels += " " + persoon.getPartnerTussenvoegsel();
		}
		else if (naamGebruik == null)
		{
			if (StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()) && StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
			{
				tussenvoegels += " " + persoon.getPartnerTussenvoegsel();
			}
			else if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
			{
				tussenvoegels += " " + persoon.getTussenvoegsel();
			}
		}
		return tussenvoegels;
	}

	public static String getVoorlettersClient(Client client)
	{
		if (client == null)
		{
			return null;
		}

		var voorletters = new StringBuilder();

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
		var naam = new StringBuilder();
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
		var naam = new StringBuilder();
		if (cervixHuisarts == null)
		{
			return naam.toString();
		}
		var medewerker = cervixHuisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
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

	public static String getGeboorteTussenvoegselEnAchternaam(GbaPersoon persoon)
	{
		return getEigennaam(persoon);
	}

	public static String getGewensteAanspreekVorm(Client client)
	{
		if (client != null)
		{
			var persoon = client.getPersoon();
			var sb = new StringBuilder();
			var aanhef = Aanhef.bepaalJuisteAanhef(persoon);
			sb.append(aanhef.getNaam()).append(" ");
			if (aanhef == Aanhef.GEACHTE)
			{
				sb.append(voorlettersTussenvoegselEnAanspreekAchternaam(client));
			}
			else
			{
				sb.append(StringUtils.capitalize(getAanspreekTussenvoegselEnAchternaam(client)));
			}
			return sb.toString();
		}
		return null;
	}
}
