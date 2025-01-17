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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaUploadBeeldenVerzoekType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "mamma", name = "upload_beelden_verzoek")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaUploadBeeldenVerzoek extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningRonde screeningRonde;

	@Column(nullable = false)
	private Date creatieDatum;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private InstellingGebruiker gemaaktDoor;

	@ManyToOne(optional = false)
	private Instelling ziekenhuis;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaUploadBeeldenVerzoekType verzoekType;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaUploadBeeldenVerzoekStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Enumerated(EnumType.STRING)
	private MammaFollowUpBIRADSWaarde conclusieBirads;

	@Column(length = 1024)
	private String conclusieEersteUitslagRadiologie;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaUploadBeeldenPoging laatsteUploadPoging;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "uploadBeeldenVerzoek", cascade = CascadeType.ALL)
	private List<MammaUploadBeeldenPoging> uploadPogingen = new ArrayList<>();

	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public InstellingGebruiker getGemaaktDoor()
	{
		return gemaaktDoor;
	}

	public void setGemaaktDoor(InstellingGebruiker gemaaktDoor)
	{
		this.gemaaktDoor = gemaaktDoor;
	}

	public Instelling getZiekenhuis()
	{
		return ziekenhuis;
	}

	public void setZiekenhuis(Instelling ziekenhuis)
	{
		this.ziekenhuis = ziekenhuis;
	}

	public MammaUploadBeeldenVerzoekType getVerzoekType()
	{
		return verzoekType;
	}

	public void setVerzoekType(MammaUploadBeeldenVerzoekType verzoekType)
	{
		this.verzoekType = verzoekType;
	}

	public MammaFollowUpBIRADSWaarde getConclusieBirads()
	{
		return conclusieBirads;
	}

	public void setConclusieBirads(MammaFollowUpBIRADSWaarde conclusieBirads)
	{
		this.conclusieBirads = conclusieBirads;
	}

	public String getConclusieEersteUitslagRadiologie()
	{
		return conclusieEersteUitslagRadiologie;
	}

	public void setConclusieEersteUitslagRadiologie(String conclusieEersteUitslagRadiologie)
	{
		this.conclusieEersteUitslagRadiologie = conclusieEersteUitslagRadiologie;
	}

	public MammaUploadBeeldenVerzoekStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaUploadBeeldenVerzoekStatus status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public MammaUploadBeeldenPoging getLaatsteUploadPoging()
	{
		return laatsteUploadPoging;
	}

	public void setLaatsteUploadPoging(MammaUploadBeeldenPoging laatsteUploadPoging)
	{
		this.laatsteUploadPoging = laatsteUploadPoging;
	}

	public List<MammaUploadBeeldenPoging> getUploadPogingen()
	{
		return uploadPogingen;
	}

	public void setUploadPogingen(List<MammaUploadBeeldenPoging> uploadPogingen)
	{
		this.uploadPogingen = uploadPogingen;
	}
}
