package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaBlokkadeType;
import nl.rivm.screenit.util.DiffSpecs;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "blokkade")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
public class MammaBlokkade extends AbstractHibernateObject implements IActief
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private Boolean actief;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private MammaBlokkadeType type;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats standplaats;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaScreeningsEenheid screeningsEenheid;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private ScreeningOrganisatie regio;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date vanaf;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date totEnMet;

	@Column(length = HibernateMagicNumber.L4096)
	private String reden;

	public MammaStandplaats getStandplaats()
	{
		return standplaats;
	}

	public void setStandplaats(MammaStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public ScreeningOrganisatie getRegio()
	{
		return regio;
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = regio;
	}

	public MammaBlokkadeType getType()
	{
		return type;
	}

	public void setType(MammaBlokkadeType blokkadeType)
	{
		this.type = blokkadeType;
	}

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTotEnMet()
	{
		return totEnMet;
	}

	public void setTotEnMet(Date totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	public String getReden()
	{
		return reden;
	}

	public void setReden(String reden)
	{
		this.reden = reden;
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
