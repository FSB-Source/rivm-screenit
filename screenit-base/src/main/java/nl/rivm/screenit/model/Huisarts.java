package nl.rivm.screenit.model;

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

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;

import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class Huisarts extends TablePerClassHibernateObject
{

	private String achternaam;

	private String voorletters;

	private String tussenvoegels;

	@OneToOne(fetch = FetchType.EAGER)
	@Cascade(CascadeType.ALL)
	private Adres adres;

	private String praktijknaam;

	private String ediadres;

	private String oorspronkelijkEdiadres;

	private Date gewijzigd;

	private String klantnummer;

	@Enumerated(EnumType.STRING)
	private HuisartsGeslacht geslacht;

	private String telefoonnummer;

	private boolean verwijderd = false;

	private String huisartsAgb;

	private String praktijkAgb;

	private String weergavenaam;

	public String getAchternaam()
	{
		return achternaam;
	}

	public void setAchternaam(String achternaam)
	{
		this.achternaam = achternaam;
	}

	public String getVoorletters()
	{
		return voorletters;
	}

	public void setVoorletters(String voorletters)
	{
		this.voorletters = voorletters;
	}

	public String getTussenvoegels()
	{
		return tussenvoegels;
	}

	public void setTussenvoegels(String tussenvoegels)
	{
		this.tussenvoegels = tussenvoegels;
	}

	public Adres getAdres()
	{
		return adres;
	}

	public void setAdres(Adres adres)
	{
		this.adres = adres;
	}

	public String getPraktijknaam()
	{
		return praktijknaam;
	}

	public void setPraktijknaam(String praktijknaam)
	{
		this.praktijknaam = praktijknaam;
	}

	public String getEdiadres()
	{
		return ediadres;
	}

	public void setEdiadres(String ediadres)
	{
		this.ediadres = ediadres;
	}

	public Date getGewijzigd()
	{
		return gewijzigd;
	}

	public void setGewijzigd(Date gewijzigd)
	{
		this.gewijzigd = gewijzigd;
	}

	public String getKlantnummer()
	{
		return klantnummer;
	}

	public void setKlantnummer(String klantnummer)
	{
		this.klantnummer = klantnummer;
	}

	public HuisartsGeslacht getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(HuisartsGeslacht geslacht)
	{
		this.geslacht = geslacht;
	}

	public String getTelefoonnummer()
	{
		return telefoonnummer;
	}

	public void setTelefoonnummer(String telefoonnummer)
	{
		this.telefoonnummer = telefoonnummer;
	}

	public boolean isVerwijderd()
	{
		return verwijderd;
	}

	public void setVerwijderd(boolean verwijderd)
	{
		this.verwijderd = verwijderd;
	}

	public String getHuisartsAgb()
	{
		return huisartsAgb;
	}

	public void setHuisartsAgb(String huisartsAgb)
	{
		this.huisartsAgb = huisartsAgb;
	}

	public String getPraktijkAgb()
	{
		return praktijkAgb;
	}

	public void setPraktijkAgb(String praktijkAgb)
	{
		this.praktijkAgb = praktijkAgb;
	}

	public String getWeergavenaam()
	{
		return weergavenaam;
	}

	public void setWeergavenaam(String weergavenaam)
	{
		this.weergavenaam = weergavenaam;
	}

	public String getOorspronkelijkEdiadres()
	{
		return oorspronkelijkEdiadres;
	}

	public void setOorspronkelijkEdiadres(String oorspronkelijkEdiadres)
	{
		this.oorspronkelijkEdiadres = oorspronkelijkEdiadres;
	}

}
