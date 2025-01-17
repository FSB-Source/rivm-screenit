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

import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "mamma", name = "adhoc_meekijkverzoek")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaAdhocMeekijkverzoek extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaOnderzoek onderzoek;

	@Column(nullable = false)
	private String reden;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private MammaVisitatieOnderzoekStatus status;

	@Column(unique = true, nullable = false)
	private Long volgnummer;

	public MammaOnderzoek getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(MammaOnderzoek onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public String getReden()
	{
		return reden;
	}

	public void setReden(String reden)
	{
		this.reden = reden;
	}

	public Long getVolgnummer()
	{
		return volgnummer;
	}

	public void setVolgnummer(Long volgnummer)
	{
		this.volgnummer = volgnummer;
	}

	public MammaVisitatieOnderzoekStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaVisitatieOnderzoekStatus status)
	{
		this.status = status;
	}

}
