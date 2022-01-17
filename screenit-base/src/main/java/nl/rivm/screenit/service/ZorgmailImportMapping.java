package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public class ZorgmailImportMapping
{

	private int klantnummer;

	private int weergavenaam;

	private int ediAdres;

	private int gewist;

	private int gewijzigd;

	private int huisartsagb;

	private int geslacht;

	private int achternaam;

	private int initialen;

	private int tussenvoegsels;

	private int praktijkagb;

	private int praktijknaam;

	private int straat;

	private int huisnummer;

	private int plaats;

	private int postcode;

	private int land;

	private int telefoonnummer;

	private int role;

	private int organisationType;

	public ZorgmailImportMapping()
	{
		setAchternaam(-1);
		setEdiAdres(-1);
		setGeslacht(-1);
		setGewijzigd(-1);
		setGewist(-1);
		setHuisartsagb(-1);
		setHuisnummer(-1);
		setInitialen(-1);
		setKlantnummer(-1);
		setLand(-1);
		setPlaats(-1);
		setPostcode(-1);
		setPraktijkagb(-1);
		setPraktijknaam(-1);
		setStraat(-1);
		setTelefoonnummer(-1);
		setTussenvoegsels(-1);
		setWeergavenaam(-1);
		setRole(-1);
		setOrganisationType(-1);
	}

	public int getKlantnummer()
	{
		return klantnummer;
	}

	public void setKlantnummer(int klantnummer)
	{
		this.klantnummer = klantnummer;
	}

	public int getWeergavenaam()
	{
		return weergavenaam;
	}

	public void setWeergavenaam(int weergavenaam)
	{
		this.weergavenaam = weergavenaam;
	}

	public int getEdiAdres()
	{
		return ediAdres;
	}

	public void setEdiAdres(int ediAdres)
	{
		this.ediAdres = ediAdres;
	}

	public int getGewist()
	{
		return gewist;
	}

	public void setGewist(int gewist)
	{
		this.gewist = gewist;
	}

	public int getGewijzigd()
	{
		return gewijzigd;
	}

	public void setGewijzigd(int gewijzigd)
	{
		this.gewijzigd = gewijzigd;
	}

	public int getHuisartsagb()
	{
		return huisartsagb;
	}

	public void setHuisartsagb(int huisartsagb)
	{
		this.huisartsagb = huisartsagb;
	}

	public int getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(int geslacht)
	{
		this.geslacht = geslacht;
	}

	public int getAchternaam()
	{
		return achternaam;
	}

	public void setAchternaam(int achternaam)
	{
		this.achternaam = achternaam;
	}

	public int getInitialen()
	{
		return initialen;
	}

	public void setInitialen(int initialen)
	{
		this.initialen = initialen;
	}

	public int getTussenvoegsels()
	{
		return tussenvoegsels;
	}

	public void setTussenvoegsels(int tussenvoegsels)
	{
		this.tussenvoegsels = tussenvoegsels;
	}

	public int getPraktijkagb()
	{
		return praktijkagb;
	}

	public void setPraktijkagb(int praktijkagb)
	{
		this.praktijkagb = praktijkagb;
	}

	public int getPraktijknaam()
	{
		return praktijknaam;
	}

	public void setPraktijknaam(int praktijknaam)
	{
		this.praktijknaam = praktijknaam;
	}

	public int getStraat()
	{
		return straat;
	}

	public void setStraat(int straat)
	{
		this.straat = straat;
	}

	public int getHuisnummer()
	{
		return huisnummer;
	}

	public void setHuisnummer(int huisnummer)
	{
		this.huisnummer = huisnummer;
	}

	public int getPlaats()
	{
		return plaats;
	}

	public void setPlaats(int plaats)
	{
		this.plaats = plaats;
	}

	public int getPostcode()
	{
		return postcode;
	}

	public void setPostcode(int postcode)
	{
		this.postcode = postcode;
	}

	public int getLand()
	{
		return land;
	}

	public void setLand(int land)
	{
		this.land = land;
	}

	public int getTelefoonnummer()
	{
		return telefoonnummer;
	}

	public void setTelefoonnummer(int telefoonnummer)
	{
		this.telefoonnummer = telefoonnummer;
	}

	public int getRole()
	{
		return role;
	}

	public void setRole(int role)
	{
		this.role = role;
	}

	public int getOrganisationType()
	{
		return organisationType;
	}

	public void setOrganisationType(int organisationType)
	{
		this.organisationType = organisationType;
	}

}
