package nl.rivm.screenit.model;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.RetourzendingAfhandelingType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class RetourredenAfhandeling extends AbstractHibernateObject
{
	@Column(nullable = false, unique = true)
	private String retourReden;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private RetourzendingAfhandelingType afhandeling;

	public String getRetourReden()
	{
		return retourReden;
	}

	public void setRetourReden(String retourReden)
	{
		this.retourReden = retourReden;
	}

	public RetourzendingAfhandelingType getAfhandeling()
	{
		return afhandeling;
	}

	public void setAfhandeling(RetourzendingAfhandelingType afhandeling)
	{
		this.afhandeling = afhandeling;
	}
}
