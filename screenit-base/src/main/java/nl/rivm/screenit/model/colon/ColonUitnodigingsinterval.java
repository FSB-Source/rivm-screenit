package nl.rivm.screenit.model.colon;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.IntervalEenheidAanduiding;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "colon", name = "uitnodigingsinterval", uniqueConstraints = { @UniqueConstraint(columnNames = "type") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ColonUitnodigingsinterval extends AbstractHibernateObject
{

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	@NotAudited
	private ColonUitnodigingsintervalType type;

	@Column(nullable = true)
	private Integer aantal;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private IntervalEenheidAanduiding eenheid;

	@Temporal(TemporalType.DATE)
	@NotAudited
	private Date berekendeReferentieDatum;

	public ColonUitnodigingsintervalType getType()
	{
		return type;
	}

	public void setType(ColonUitnodigingsintervalType type)
	{
		this.type = type;
	}

	public Integer getAantal()
	{
		return aantal;
	}

	public void setAantal(Integer aantal)
	{
		this.aantal = aantal;
	}

	public IntervalEenheidAanduiding getEenheid()
	{
		return eenheid;
	}

	public void setEenheid(IntervalEenheidAanduiding eenheid)
	{
		this.eenheid = eenheid;
	}

	public Date getBerekendeReferentieDatum()
	{
		return berekendeReferentieDatum;
	}

	public void setBerekendeReferentieDatum(Date berekendeReferentieDatum)
	{
		this.berekendeReferentieDatum = berekendeReferentieDatum;
	}
}
