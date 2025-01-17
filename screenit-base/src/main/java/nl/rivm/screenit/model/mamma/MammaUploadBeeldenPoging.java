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

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
@Audited
@Table(schema = "mamma", name = "upload_beelden_poging")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaUploadBeeldenPoging extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaUploadBeeldenVerzoek uploadBeeldenVerzoek;

	private Long accessionNumber;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaMammografieIlmStatus ilmStatus;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date ilmStatusDatum;

	@Column(length = 1024)
	private String statusMelding;

	@OneToMany(fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	@JoinTable(schema = "mamma", name = "upload_beelden_poging_documents", joinColumns = { @JoinColumn(name = "upload_beelden_poging") })
	private List<UploadDocument> bestanden = new ArrayList<>();

	public MammaUploadBeeldenVerzoek getUploadBeeldenVerzoek()
	{
		return uploadBeeldenVerzoek;
	}

	public void setUploadBeeldenVerzoek(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek)
	{
		this.uploadBeeldenVerzoek = uploadBeeldenVerzoek;
	}

	public Long getAccessionNumber()
	{
		return accessionNumber;
	}

	public void setAccessionNumber(Long accessionNumber)
	{
		this.accessionNumber = accessionNumber;
	}

	public MammaMammografieIlmStatus getIlmStatus()
	{
		return ilmStatus;
	}

	public void setIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		this.ilmStatus = ilmStatus;
	}

	public Date getIlmStatusDatum()
	{
		return ilmStatusDatum;
	}

	public void setIlmStatusDatum(Date ilmStatusDatum)
	{
		this.ilmStatusDatum = ilmStatusDatum;
	}

	public String getStatusMelding()
	{
		return statusMelding;
	}

	public void setStatusMelding(String statusMelding)
	{
		this.statusMelding = statusMelding;
	}

	public List<UploadDocument> getBestanden()
	{
		return bestanden;
	}

	public void setBestanden(List<UploadDocument> bestanden)
	{
		this.bestanden = bestanden;
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}
}
