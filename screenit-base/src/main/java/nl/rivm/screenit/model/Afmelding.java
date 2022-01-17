
package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfmelding;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class Afmelding<SR extends ScreeningRonde<?, ?, ?, ?>, D extends Dossier<?, ?>, B extends ClientBrief<?, ?, ?>> extends TablePerClassHibernateObject
{

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date afmeldDatum;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AfmeldingType type;

	@Transient
	private ClientContactManier manier;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AanvraagBriefStatus afmeldingStatus;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusAfmeldDatum;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.ALL)
	@NotAudited
	private UploadDocument handtekeningDocumentAfmelding;

	@Column(nullable = false)
	private Boolean rondeGesloten = false;

	@Column(nullable = false)
	private Boolean rondeHeropend = false;

	@Temporal(TemporalType.TIMESTAMP)
	private Date heraanmeldDatum;

	@Enumerated(EnumType.STRING)
	private AanvraagBriefStatus heraanmeldStatus;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusHeraanmeldDatum;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.ALL)
	@NotAudited
	private UploadDocument handtekeningDocumentHeraanmelding;

	private Boolean clientWilNieuweUitnodiging;

	private Boolean implicieteAfmelding = false;

	private Boolean implicieteHeraanmelding = false;

	public AfmeldingType getType()
	{
		return type;
	}

	public void setType(AfmeldingType type)
	{
		this.type = type;
	}

	public AanvraagBriefStatus getAfmeldingStatus()
	{
		return afmeldingStatus;
	}

	public void setAfmeldingStatus(AanvraagBriefStatus status)
	{
		this.afmeldingStatus = status;
	}

	public Date getStatusHeraanmeldDatum()
	{
		return statusHeraanmeldDatum;
	}

	public void setStatusHeraanmeldDatum(Date statusHeraanmeldDatum)
	{
		this.statusHeraanmeldDatum = statusHeraanmeldDatum;
	}

	public AanvraagBriefStatus getHeraanmeldStatus()
	{
		return heraanmeldStatus;
	}

	public void setHeraanmeldStatus(AanvraagBriefStatus heraanmeldStatus)
	{
		this.heraanmeldStatus = heraanmeldStatus;
	}

	public Boolean getRondeGesloten()
	{
		return rondeGesloten;
	}

	public void setRondeGesloten(Boolean rondeGesloten)
	{
		this.rondeGesloten = rondeGesloten;
	}

	public Date getAfmeldDatum()
	{
		return afmeldDatum;
	}

	public void setAfmeldDatum(Date afmeldDatum)
	{
		this.afmeldDatum = afmeldDatum;
	}

	public Date getHeraanmeldDatum()
	{
		return heraanmeldDatum;
	}

	public void setHeraanmeldDatum(Date heraanmeldDatum)
	{
		this.heraanmeldDatum = heraanmeldDatum;
	}

	public Boolean getRondeHeropend()
	{
		return rondeHeropend;
	}

	public void setRondeHeropend(Boolean rondeHeropend)
	{
		this.rondeHeropend = rondeHeropend;
	}

	public UploadDocument getHandtekeningDocumentAfmelding()
	{
		return handtekeningDocumentAfmelding;
	}

	public void setHandtekeningDocumentAfmelding(UploadDocument handtekeningDocument)
	{
		this.handtekeningDocumentAfmelding = handtekeningDocument;
	}

	public UploadDocument getHandtekeningDocumentHeraanmelding()
	{
		return handtekeningDocumentHeraanmelding;
	}

	public void setHandtekeningDocumentHeraanmelding(UploadDocument handtekeningDocumentHeraanmelding)
	{
		this.handtekeningDocumentHeraanmelding = handtekeningDocumentHeraanmelding;
	}

	public void setClientWilNieuweUitnodiging(Boolean clientWilNieuweUitnodiging)
	{
		this.clientWilNieuweUitnodiging = clientWilNieuweUitnodiging;
	}

	public Date getStatusAfmeldDatum()
	{
		return statusAfmeldDatum;
	}

	public void setStatusAfmeldDatum(Date statusAfmeldDatum)
	{
		this.statusAfmeldDatum = statusAfmeldDatum;
	}

	public ClientContactManier getManier()
	{
		return manier;
	}

	public void setManier(ClientContactManier manier)
	{
		this.manier = manier;
	}

	public Boolean getClientWilNieuweUitnodiging()
	{
		return clientWilNieuweUitnodiging;
	}

	public abstract B getAfmeldingAanvraag();

	public abstract void setAfmeldingAanvraag(B afmeldingAanvraag);

	public abstract B getAfmeldingBevestiging();

	public abstract void setAfmeldingBevestiging(B afmeldingBevestiging);

	public abstract B getHeraanmeldAanvraag();

	public abstract void setHeraanmeldAanvraag(B heraanmeldAanvraag);

	public abstract B getHeraanmeldBevestiging();

	public abstract void setHeraanmeldBevestiging(B heraanmeldBevestiging);

	public abstract List<B> getBrieven();

	public abstract void setBrieven(List<B> brieven);

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);

	public abstract D getDossier();

	public abstract void setDossier(D dossier);

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		Class aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (aClass == ColonAfmelding.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (aClass == CervixAfmelding.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (aClass == MammaAfmelding.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}

	public Boolean getImplicieteAfmelding()
	{
		return implicieteAfmelding;
	}

	public void setImplicieteAfmelding(Boolean implicieteAfmelding)
	{
		this.implicieteAfmelding = implicieteAfmelding;
	}

	public Boolean getImplicieteHeraanmelding()
	{
		return implicieteHeraanmelding;
	}

	public void setImplicieteHeraanmelding(Boolean implicieteHeraanmelding)
	{
		this.implicieteHeraanmelding = implicieteHeraanmelding;
	}
}
