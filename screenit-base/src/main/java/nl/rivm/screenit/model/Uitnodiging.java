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

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import java.util.Date;

@Entity
@Table
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class Uitnodiging<SR extends ScreeningRonde<?, ?, ?, ?>> extends TablePerClassHibernateObject
{

	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@Temporal(TemporalType.DATE)
	private Date uitnodigingsDatum;

	public Uitnodiging()
	{
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public Date getUitnodigingsDatum()
	{
		return uitnodigingsDatum;
	}

	public void setUitnodigingsDatum(Date uitnodigingsDatum)
	{
		this.uitnodigingsDatum = uitnodigingsDatum;
	}

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);

}
