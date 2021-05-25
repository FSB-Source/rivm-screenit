package nl.rivm.screenit.model.cervix.facturatie;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.SingleTableHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "tarief")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class CervixTarief extends SingleTableHibernateObject implements IActief
{

	@Column(nullable = false)
	private Boolean actief = Boolean.TRUE;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date geldigVanafDatum;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date geldigTotenmetDatum;

	public Date getGeldigVanafDatum()
	{
		return geldigVanafDatum;
	}

	public void setGeldigVanafDatum(Date geldigVanafDatum)
	{
		this.geldigVanafDatum = geldigVanafDatum;
	}

	public Date getGeldigTotenmetDatum()
	{
		return geldigTotenmetDatum;
	}

	public void setGeldigTotenmetDatum(Date geldigTotenmetDatum)
	{
		this.geldigTotenmetDatum = geldigTotenmetDatum;
	}

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}
}
