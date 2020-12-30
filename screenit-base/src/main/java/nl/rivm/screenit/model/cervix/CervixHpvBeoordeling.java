package nl.rivm.screenit.model.cervix;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "cervix", name = "hpv_beoordeling", indexes = { @Index(name = "idx_CERVIX_HPV_BEOORDELING_HPV_UITSLAG", columnList = "hpvUitslag") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class CervixHpvBeoordeling extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private Date analyseDatum;

	@Column(nullable = false)
	private Date autorisatieDatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixHpvUitslag hpvUitslag;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixHpvBericht hpvBericht;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixMonster monster;

	public CervixHpvBericht getHpvBericht()
	{
		return hpvBericht;
	}

	public void setHpvBericht(CervixHpvBericht hpvBericht)
	{
		this.hpvBericht = hpvBericht;
	}

	public CervixMonster getMonster()
	{
		return monster;
	}

	public void setMonster(CervixMonster monster)
	{
		this.monster = monster;
	}

	public Date getAnalyseDatum()
	{
		return analyseDatum;
	}

	public void setAnalyseDatum(Date analyseDatum)
	{
		this.analyseDatum = analyseDatum;
	}

	public Date getAutorisatieDatum()
	{
		return autorisatieDatum;
	}

	public void setAutorisatieDatum(Date autorisatieDatum)
	{
		this.autorisatieDatum = autorisatieDatum;
	}

	public CervixHpvUitslag getHpvUitslag()
	{
		return hpvUitslag;
	}

	public void setHpvUitslag(CervixHpvUitslag hpvUitslag)
	{
		this.hpvUitslag = hpvUitslag;
	}
}
