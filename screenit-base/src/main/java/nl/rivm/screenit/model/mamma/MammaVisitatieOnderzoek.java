package nl.rivm.screenit.model.mamma;

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
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Table(
	schema = "mamma",
	name = "visitatie_onderzoek",
	uniqueConstraints = { @UniqueConstraint(columnNames = { "visitatie", "onderdeel", "volgnummer" }, name = "uc_mamma_visitatie_onderdeel_volgnummer") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaVisitatieOnderzoek extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaBeoordeling beoordeling;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaVisitatie visitatie;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaVisitatieOnderzoekStatus status;

	@Column(nullable = false)
	@NotAudited
	private Integer volgnummer;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaVisitatieOnderdeel onderdeel;

	public MammaBeoordeling getBeoordeling()
	{
		return beoordeling;
	}

	public void setBeoordeling(MammaBeoordeling beoordeling)
	{
		this.beoordeling = beoordeling;
	}

	public MammaVisitatie getVisitatie()
	{
		return visitatie;
	}

	public void setVisitatie(MammaVisitatie vistiatie)
	{
		this.visitatie = vistiatie;
	}

	public MammaVisitatieOnderzoekStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaVisitatieOnderzoekStatus status)
	{
		this.status = status;
	}

	public Integer getVolgnummer()
	{
		return volgnummer;
	}

	public void setVolgnummer(Integer volgnummer)
	{
		this.volgnummer = volgnummer;
	}

	public MammaVisitatieOnderdeel getOnderdeel()
	{
		return onderdeel;
	}

	public void setOnderdeel(MammaVisitatieOnderdeel onderdeel)
	{
		this.onderdeel = onderdeel;
	}
}
