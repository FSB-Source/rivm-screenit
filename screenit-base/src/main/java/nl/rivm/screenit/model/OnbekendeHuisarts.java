package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinTable;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class OnbekendeHuisarts extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@JoinTable(schema = "algemeen", name = "onbekende_huisarts_huisarts_berichten")
	private List<ColonHuisartsBericht> huisartsBerichten;

	private String huisartsNaam;

	private String praktijkNaam;

	private String praktijkAdres;

	private String praktijkPostcode;

	private String praktijkPlaats;

	private String telefoonnummer;

	private String faxnummer;

	public String getHuisartsNaam()
	{
		return huisartsNaam;
	}

	public void setHuisartsNaam(String huisartsNaam)
	{
		this.huisartsNaam = huisartsNaam;
	}

	public String getPraktijkNaam()
	{
		return praktijkNaam;
	}

	public void setPraktijkNaam(String praktijkNaam)
	{
		this.praktijkNaam = praktijkNaam;
	}

	public String getPraktijkAdres()
	{
		return praktijkAdres;
	}

	public void setPraktijkAdres(String praktijkAdres)
	{
		this.praktijkAdres = praktijkAdres;
	}

	public String getFaxnummer()
	{
		return faxnummer;
	}

	public void setFaxnummer(String faxnummer)
	{
		this.faxnummer = faxnummer;
	}

	public String getTelefoonnummer()
	{
		return telefoonnummer;
	}

	public void setTelefoonnummer(String telefoonnummer)
	{
		this.telefoonnummer = telefoonnummer;
	}

	public String getPraktijkPostcode()
	{
		return praktijkPostcode;
	}

	public void setPraktijkPostcode(String praktijkPostcode)
	{
		this.praktijkPostcode = praktijkPostcode;
	}

	public String getPraktijkPlaats()
	{
		return praktijkPlaats;
	}

	public void setPraktijkPlaats(String praktijkPlaats)
	{
		this.praktijkPlaats = praktijkPlaats;
	}

	public List<ColonHuisartsBericht> getHuisartsBerichten()
	{
		return huisartsBerichten;
	}

	public void setHuisartsBerichten(List<ColonHuisartsBericht> huisartsBerichten)
	{
		this.huisartsBerichten = huisartsBerichten;
	}
}
