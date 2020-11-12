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

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class GbaVerwerkingEntry extends AbstractHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	private Long screeningOrganisatie;

	private Integer aantalNieuweBurgers = 0;

	private Integer aantalBijgewerkteBugers = 0;

	@ManyToOne
	private GbaVerwerkingsLog verwerkingsLog;

	public Long getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(Long screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

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

	public GbaVerwerkingsLog getVerwerkingsLog()
	{
		return verwerkingsLog;
	}

	public void setVerwerkingsLog(GbaVerwerkingsLog verwerkingsLog)
	{
		this.verwerkingsLog = verwerkingsLog;
	}
}
