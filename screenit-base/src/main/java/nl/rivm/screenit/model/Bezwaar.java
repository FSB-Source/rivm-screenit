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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class Bezwaar extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private BezwaarType type;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private Bevolkingsonderzoek bevolkingsonderzoek;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private BezwaarMoment bezwaarMoment;

	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return bevolkingsonderzoek;
	}

	public void setBevolkingsonderzoek(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		this.bevolkingsonderzoek = bevolkingsonderzoek;
	}

	public BezwaarMoment getBezwaarMoment()
	{
		return bezwaarMoment;
	}

	public void setBezwaarMoment(BezwaarMoment bezwaarMoment)
	{
		this.bezwaarMoment = bezwaarMoment;
	}

	public void setType(BezwaarType type)
	{
		this.type = type;
	}

	public BezwaarType getType()
	{
		return type;
	}
}
