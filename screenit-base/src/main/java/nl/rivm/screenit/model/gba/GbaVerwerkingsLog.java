
package nl.rivm.screenit.model.gba;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class GbaVerwerkingsLog extends AbstractHibernateObject
{
	@Temporal(TemporalType.TIMESTAMP)
	@Column
	private Date datumVerwerking;

	@Column
	private Integer aantalNieuweBurgers = 0;

	@Column
	private Integer aantalBijgewerkteBugers = 0;

	@Column
	private Long aantalNieuweColonDossiers = 0L;

	@Column
	private Long aantalNieuweCervixDossiers = 0L;

	@Column
	private Long aantalNieuweMammaDossiers = 0L;

	@OneToMany(mappedBy = "verwerkingsLog")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@Cascade(org.hibernate.annotations.CascadeType.DELETE)
	private List<GbaVerwerkingEntry> entries = new ArrayList<>();

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "verwerkingsLog")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<GbaFoutRegel> fouten = new ArrayList<>();

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "gbaVerwerkingsLog")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<GbaFile> bestanden = new ArrayList<>();

	public Integer getAantalNieuweBurgers()
	{
		return aantalNieuweBurgers;
	}

	public void setAantalNieuweBurgers(Integer aantalNieuweBurgers)
	{
		this.aantalNieuweBurgers = aantalNieuweBurgers;
	}

	public Integer getAantalBijgewerkteBugers()
	{
		return aantalBijgewerkteBugers;
	}

	public void setAantalBijgewerkteBugers(Integer aantalBijgewerkteBugers)
	{
		this.aantalBijgewerkteBugers = aantalBijgewerkteBugers;
	}

	public List<GbaFoutRegel> getFouten()
	{
		return fouten;
	}

	public void setFouten(List<GbaFoutRegel> fouten)
	{
		this.fouten = fouten;
	}

	public Date getDatumVerwerking()
	{
		return datumVerwerking;
	}

	public void setDatumVerwerking(Date datumVerwerking)
	{
		this.datumVerwerking = datumVerwerking;
	}

	public List<GbaVerwerkingEntry> getEntries()
	{
		return entries;
	}

	public void setEntries(List<GbaVerwerkingEntry> entries)
	{
		this.entries = entries;
	}

	public List<GbaFile> getBestanden()
	{
		return bestanden;
	}

	public void setBestanden(List<GbaFile> bestanden)
	{
		this.bestanden = bestanden;
	}

	public Long getAantalNieuweColonDossiers()
	{
		return aantalNieuweColonDossiers;
	}

	public void setAantalNieuweColonDossiers(Long aantalNieuweColonDossiers)
	{
		this.aantalNieuweColonDossiers = aantalNieuweColonDossiers;
	}

	public Long getAantalNieuweCervixDossiers()
	{
		return aantalNieuweCervixDossiers;
	}

	public void setAantalNieuweCervixDossiers(Long aantalNieuweCervixDossiers)
	{
		this.aantalNieuweCervixDossiers = aantalNieuweCervixDossiers;
	}

	public Long getAantalNieuweMammaDossiers()
	{
		return aantalNieuweMammaDossiers;
	}

	public void setAantalNieuweMammaDossiers(Long aantalNieuweMammaDossiers)
	{
		this.aantalNieuweMammaDossiers = aantalNieuweMammaDossiers;
	}
}
