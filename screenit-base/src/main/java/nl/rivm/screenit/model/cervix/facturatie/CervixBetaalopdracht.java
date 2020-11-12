package nl.rivm.screenit.model.cervix.facturatie;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "cervix",
	name = "betaalopdracht",
	uniqueConstraints = { @UniqueConstraint(columnNames = "sepa_specificatie_pdf"), @UniqueConstraint(columnNames = "sepa_document") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixBetaalopdracht extends AbstractHibernateObject
{
	@Column(nullable = false)
	private String vanIban;

	@Column(nullable = true)
	private String vanTenaamstelling;

	@Column(nullable = false)
	private String betalingskenmerk;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BestandStatus status;

	@OneToMany(mappedBy = "betaalopdracht", fetch = FetchType.LAZY, cascade = CascadeType.REMOVE)
	private List<CervixBetaalopdrachtRegel> betaalopdrachtRegels = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	private UploadDocument sepaSpecificatiePdf;

	@Column(nullable = true)
	private String hashtotaal;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	private UploadDocument sepaDocument;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ScreeningOrganisatie screeningOrganisatie;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	private String omschrijving;

	public String getHashtotaal()
	{
		return hashtotaal;
	}

	public void setHashtotaal(String hashtotaal)
	{
		this.hashtotaal = hashtotaal;
	}

	public String getVanIban()
	{
		return vanIban;
	}

	public void setVanIban(String vanIban)
	{
		this.vanIban = vanIban;
	}

	public String getBetalingskenmerk()
	{
		return betalingskenmerk;
	}

	public void setBetalingskenmerk(String betalingskenmerk)
	{
		this.betalingskenmerk = betalingskenmerk;
	}

	public List<CervixBetaalopdrachtRegel> getBetaalopdrachtRegels()
	{
		return betaalopdrachtRegels;
	}

	public void setBetaalopdrachtRegels(List<CervixBetaalopdrachtRegel> betaalopdrachtRegels)
	{
		this.betaalopdrachtRegels = betaalopdrachtRegels;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie organisatie)
	{
		this.screeningOrganisatie = organisatie;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public UploadDocument getSepaSpecificatiePdf()
	{
		return sepaSpecificatiePdf;
	}

	public void setSepaSpecificatiePdf(UploadDocument sepaSpecificatiePdf)
	{
		this.sepaSpecificatiePdf = sepaSpecificatiePdf;
	}

	public UploadDocument getSepaDocument()
	{
		return sepaDocument;
	}

	public void setSepaDocument(UploadDocument sepaDocument)
	{
		this.sepaDocument = sepaDocument;
	}

	public String getOmschrijving()
	{
		return omschrijving;
	}

	public void setOmschrijving(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	public String getVanTenaamstelling()
	{
		return vanTenaamstelling;
	}

	public void setVanTenaamstelling(String tenaamstelling)
	{
		this.vanTenaamstelling = tenaamstelling;
	}

	public BestandStatus getStatus()
	{
		return status;
	}

	public void setStatus(BestandStatus status)
	{
		this.status = status;
	}
}
