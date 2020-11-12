
package nl.rivm.screenit.model;

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
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class UitnodigingsGebied extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToOne(cascade = CascadeType.ALL)
	private PostcodeGebied postcodeGebied;

	private String woonplaats;

	private String gemeenteDeel;

	private String naam;

	private Integer percentageIFobtRetour;

	private Integer percentageOngunstigeIfobt;

	@ManyToOne(optional = false)
	private Gemeente gemeente;

	@OneToMany(mappedBy = "uitnodigingsGebied", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling = new ArrayList<>();

	public PostcodeGebied getPostcodeGebied()
	{
		return postcodeGebied;
	}

	public void setPostcodeGebied(PostcodeGebied postcodeGebied)
	{
		this.postcodeGebied = postcodeGebied;
	}

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public Integer getPercentageIFobtRetour()
	{
		return percentageIFobtRetour;
	}

	public void setPercentageIFobtRetour(Integer percentageIFobtRetour)
	{
		this.percentageIFobtRetour = percentageIFobtRetour;
	}

	public String getWoonplaats()
	{
		return woonplaats;
	}

	public void setWoonplaats(String woonplaats)
	{
		this.woonplaats = woonplaats;
	}

	public String getGemeenteDeel()
	{
		return gemeenteDeel;
	}

	public void setGemeenteDeel(String gemeenteDeel)
	{
		this.gemeenteDeel = gemeenteDeel;
	}

	public Gemeente getGemeente()
	{
		return gemeente;
	}

	public void setGemeente(Gemeente gemeente)
	{
		this.gemeente = gemeente;
	}

	public List<ColoscopieCentrumColonCapaciteitVerdeling> getVerdeling()
	{
		return verdeling;
	}

	public void setVerdeling(List<ColoscopieCentrumColonCapaciteitVerdeling> verdeling)
	{
		this.verdeling = verdeling;
	}

	public Integer getPercentageOngunstigeIfobt()
	{
		return percentageOngunstigeIfobt;
	}

	public void setPercentageOngunstigeIfobt(Integer percentageOngunstigeIfobt)
	{
		this.percentageOngunstigeIfobt = percentageOngunstigeIfobt;
	}
}
