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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "betaalopdracht_regel")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixBetaalopdrachtRegel extends AbstractHibernateObject
{

	@Column(nullable = false)
	private BigDecimal bedrag;

	@Column(nullable = false)
	private String naarIban;

	@Column(nullable = false)
	private String naarTenaamstelling;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixBetaalopdracht betaalopdracht;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private BMHKLaboratorium laboratorium;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixHuisartsLocatie huisartsLocatie;

	@OneToMany(mappedBy = "betaalopdrachtRegel", fetch = FetchType.LAZY, cascade = CascadeType.REMOVE)
	private List<CervixBetaalopdrachtRegelSpecificatie> specificaties = new ArrayList<>();

	public CervixBetaalopdracht getBetaalopdracht()
	{
		return betaalopdracht;
	}

	public void setBetaalopdracht(CervixBetaalopdracht betaalopdracht)
	{
		this.betaalopdracht = betaalopdracht;
	}

	public BigDecimal getBedrag()
	{
		return bedrag;
	}

	public void setBedrag(BigDecimal totaalBedrag)
	{
		this.bedrag = totaalBedrag;
	}

	public String getNaarIban()
	{
		return naarIban;
	}

	public void setNaarIban(String naarIban)
	{
		this.naarIban = naarIban;
	}

	public String getNaarTenaamstelling()
	{
		return naarTenaamstelling;
	}

	public void setNaarTenaamstelling(String tenaamstelling)
	{
		this.naarTenaamstelling = tenaamstelling;
	}

	public CervixHuisartsLocatie getHuisartsLocatie()
	{
		return huisartsLocatie;
	}

	public void setHuisartsLocatie(CervixHuisartsLocatie huisartsLocatie)
	{
		this.huisartsLocatie = huisartsLocatie;
	}

	public BMHKLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public List<CervixBetaalopdrachtRegelSpecificatie> getSpecificaties()
	{
		return specificaties;
	}

	public void setSpecificaties(List<CervixBetaalopdrachtRegelSpecificatie> specificaties)
	{
		this.specificaties = specificaties;
	}

	@Override
	protected boolean concreateEquals(AbstractHibernateObject obj)
	{
		CervixBetaalopdrachtRegel compareBetaalOpdrachtRegel = (CervixBetaalopdrachtRegel) obj;
		if (betaalopdracht == null)
		{
			return false;
		}
		else if (!betaalopdracht.equals(compareBetaalOpdrachtRegel.getBetaalopdracht()))
		{
			return false;
		}
		else if (huisartsLocatie != null && huisartsLocatie.equals(compareBetaalOpdrachtRegel.getHuisartsLocatie()))
		{
			return true;
		}
		else if (laboratorium != null && laboratorium.equals(compareBetaalOpdrachtRegel.getLaboratorium()))
		{
			return true;
		}
		else
		{
			return false;
		}

	}
}
