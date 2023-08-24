package nl.rivm.screenit.model.cervix.facturatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "boek_regel")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixBoekRegel extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private Boolean debet; 

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixVerrichting verrichting;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixTarief tarief;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBetaalopdrachtRegelSpecificatie specificatie;

	public CervixVerrichting getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(CervixVerrichting cervixVerrichting)
	{
		this.verrichting = cervixVerrichting;
	}

	public CervixTarief getTarief()
	{
		return tarief;
	}

	public void setTarief(CervixTarief tarief)
	{
		this.tarief = tarief;
	}

	public CervixBetaalopdrachtRegelSpecificatie getSpecificatie()
	{
		return specificatie;
	}

	public void setSpecificatie(CervixBetaalopdrachtRegelSpecificatie specificatie)
	{
		this.specificatie = specificatie;
	}

	public Boolean getDebet()
	{
		return debet;
	}

	public void setDebet(Boolean debet)
	{
		this.debet = debet;
	}
}
