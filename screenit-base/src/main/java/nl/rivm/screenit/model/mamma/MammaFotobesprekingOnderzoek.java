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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Table(
	schema = "mamma",
	name = "fotobespreking_onderzoek",
	uniqueConstraints = { @UniqueConstraint(columnNames = { "fotobespreking", "volgnummer" }, name = "uc_mamma_fotobespreking_volgnummer") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaFotobesprekingOnderzoek extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaBeoordeling beoordeling;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaFotobespreking fotobespreking;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaFotobesprekingOnderzoekStatus status;

	@Column(nullable = false)
	@NotAudited
	private Integer volgnummer;

	public MammaFotobespreking getFotobespreking()
	{
		return fotobespreking;
	}

	public void setFotobespreking(MammaFotobespreking fotobespreking)
	{
		this.fotobespreking = fotobespreking;
	}

	public MammaFotobesprekingOnderzoekStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaFotobesprekingOnderzoekStatus status)
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

	public MammaBeoordeling getBeoordeling()
	{
		return beoordeling;
	}

	public void setBeoordeling(MammaBeoordeling beoordeling)
	{
		this.beoordeling = beoordeling;
	}
}
