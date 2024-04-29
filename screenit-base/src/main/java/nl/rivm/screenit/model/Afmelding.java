package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfmelding;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
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

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = CascadeType.ALL)
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

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = CascadeType.ALL)
	@NotAudited
	private UploadDocument handtekeningDocumentHeraanmelding;

	private Boolean clientWilNieuweUitnodiging;

	private Boolean implicieteAfmelding = false;

	private Boolean implicieteHeraanmelding = false;

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
		var aClass = Hibernate.getClass(this);
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
}
