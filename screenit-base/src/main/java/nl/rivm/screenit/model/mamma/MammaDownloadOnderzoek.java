package nl.rivm.screenit.model.mamma;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.BestandStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Table(schema = "mamma", name = "download_onderzoek")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaDownloadOnderzoek extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaOnderzoek onderzoek;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaDownloadOnderzoekenVerzoek verzoek;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BestandStatus status;

	@Column(length = 1024)
	@NotAudited
	private String statusMelding;

	public MammaOnderzoek getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(MammaOnderzoek onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public MammaDownloadOnderzoekenVerzoek getVerzoek()
	{
		return verzoek;
	}

	public void setVerzoek(MammaDownloadOnderzoekenVerzoek verzoek)
	{
		this.verzoek = verzoek;
	}

	public BestandStatus getStatus()
	{
		return status;
	}

	public void setStatus(BestandStatus status)
	{
		this.status = status;
	}

	public String getStatusMelding()
	{
		return statusMelding;
	}

	public void setStatusMelding(String statusMelding)
	{
		this.statusMelding = statusMelding;
	}

}
