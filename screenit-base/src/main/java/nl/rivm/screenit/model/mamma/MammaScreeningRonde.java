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

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.util.SkipFieldForDiff;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "screening_ronde",
	indexes = {
		@Index(name = "idx_MAMMA_SCREENING_RONDE_STATUS", columnList = "status"),
		@Index(name = "idx_MAMMA_SCREENING_RONDE_CREATIE_DATUM", columnList = "creatieDatum") },
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "laatste_afmelding"),
		@UniqueConstraint(columnNames = "laatste_brief"),
		@UniqueConstraint(columnNames = "laatste_uitnodiging"),
		@UniqueConstraint(columnNames = "laatste_uitstel"),
		@UniqueConstraint(columnNames = "screening_ronde_event") })
@Audited
public class MammaScreeningRonde extends ScreeningRonde<MammaDossier, MammaBrief, MammaAfmelding, MammaUitnodiging>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaDossier dossier;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaUitnodiging> uitnodigingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private MammaUitnodiging laatsteUitnodiging;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaUitstel> uitstellen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaUitstel laatsteUitstel;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaBrief> brieven = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private MammaBrief laatsteBrief;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaAfmelding laatsteAfmelding;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsRonde standplaatsRonde;

	@Column(unique = true, nullable = false)
	private Long uitnodigingsNr;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Audited(targetAuditMode = NOT_AUDITED)
	@Cascade(org.hibernate.annotations.CascadeType.SAVE_UPDATE)
	private EnovationHuisarts huisarts;

	@Column
	@Enumerated(EnumType.STRING)
	private MammaGeenHuisartsOption geenHuisartsOptie;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVastleggenHuisarts;

	@Column(nullable = false)
	private boolean isGeforceerd;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	private MammaKansberekeningScreeningRondeEvent screeningRondeEvent;

	@Column(nullable = true, length = 6)
	@SkipFieldForDiff
	private String postcode;

	@Column(nullable = false)
	private Boolean minderValideOnderzoekZiekenhuis;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaFollowUpConclusieStatus followUpConclusieStatus;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date followUpConclusieStatusGewijzigdOp;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaFollowUpVerslag> followUpVerslagen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaFollowUpRadiologieVerslag> followUpRadiologieVerslagen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoeken = new ArrayList<>();

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date laatstGebeldFollowUpNietGedownload;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaOnderzoek laatsteOnderzoek;

	@Override
	public MammaDossier getDossier()
	{
		return dossier;
	}

	@Override
	public void setDossier(MammaDossier dossier)
	{
		this.dossier = dossier;
	}

	@Override
	public List<MammaUitnodiging> getUitnodigingen()
	{
		return uitnodigingen;
	}

	@Override
	public void setUitnodigingen(List<MammaUitnodiging> uitnodigingen)
	{
		this.uitnodigingen = uitnodigingen;
	}

	@Override
	public MammaUitnodiging getLaatsteUitnodiging()
	{
		return laatsteUitnodiging;
	}

	@Override
	public void setLaatsteUitnodiging(MammaUitnodiging laatsteUitnodiging)
	{
		this.laatsteUitnodiging = laatsteUitnodiging;
	}

	@Override
	public List<MammaBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<MammaBrief> brieven)
	{
		this.brieven = brieven;
	}

	@Override
	public MammaBrief getLaatsteBrief()
	{
		return laatsteBrief;
	}

	@Override
	public void setLaatsteBrief(MammaBrief laatsteBrief)
	{
		this.laatsteBrief = laatsteBrief;
	}

	@Override
	public List<MammaAfmelding> getAfmeldingen()
	{
		return afmeldingen;
	}

	@Override
	public void setAfmeldingen(List<MammaAfmelding> afmeldingen)
	{
		this.afmeldingen = afmeldingen;
	}

	@Override
	public MammaAfmelding getLaatsteAfmelding()
	{
		return laatsteAfmelding;
	}

	@Override
	public void setLaatsteAfmelding(MammaAfmelding laatsteAfmelding)
	{
		this.laatsteAfmelding = laatsteAfmelding;
	}

	public MammaStandplaatsRonde getStandplaatsRonde()
	{
		return standplaatsRonde;
	}

	public void setStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		this.standplaatsRonde = standplaatsRonde;
	}

	public Long getUitnodigingsNr()
	{
		return uitnodigingsNr;
	}

	public void setUitnodigingsNr(Long uitnodigingsNr)
	{
		this.uitnodigingsNr = uitnodigingsNr;
	}

	public EnovationHuisarts getHuisarts()
	{
		return huisarts;
	}

	public void setHuisarts(EnovationHuisarts huisarts)
	{
		this.huisarts = huisarts;
	}

	public MammaGeenHuisartsOption getGeenHuisartsOptie()
	{
		return geenHuisartsOptie;
	}

	public void setGeenHuisartsOptie(MammaGeenHuisartsOption geenHuisartsOptie)
	{
		this.geenHuisartsOptie = geenHuisartsOptie;
	}

	public boolean getIsGeforceerd()
	{
		return isGeforceerd;
	}

	public void setIsGeforceerd(boolean geforceerd)
	{
		isGeforceerd = geforceerd;
	}

	public MammaKansberekeningScreeningRondeEvent getScreeningRondeEvent()
	{
		return screeningRondeEvent;
	}

	public void setScreeningRondeEvent(MammaKansberekeningScreeningRondeEvent screeningRondeEvent)
	{
		this.screeningRondeEvent = screeningRondeEvent;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public Date getDatumVastleggenHuisarts()
	{
		return datumVastleggenHuisarts;
	}

	public void setDatumVastleggenHuisarts(Date datumVastleggenHuisarts)
	{
		this.datumVastleggenHuisarts = datumVastleggenHuisarts;
	}

	public Boolean getMinderValideOnderzoekZiekenhuis()
	{
		return minderValideOnderzoekZiekenhuis;
	}

	public void setMinderValideOnderzoekZiekenhuis(Boolean minderValideOnderzoekZiekenhuis)
	{
		this.minderValideOnderzoekZiekenhuis = minderValideOnderzoekZiekenhuis;
	}

	public List<MammaUitstel> getUitstellen()
	{
		return uitstellen;
	}

	public void setUitstellen(List<MammaUitstel> uitstellen)
	{
		this.uitstellen = uitstellen;
	}

	public MammaUitstel getLaatsteUitstel()
	{
		return laatsteUitstel;
	}

	public void setLaatsteUitstel(MammaUitstel laatsteUitstel)
	{
		this.laatsteUitstel = laatsteUitstel;
	}

	public MammaFollowUpConclusieStatus getFollowUpConclusieStatus()
	{
		return followUpConclusieStatus;
	}

	public void setFollowUpConclusieStatus(MammaFollowUpConclusieStatus followUpConclusieStatus)
	{
		this.followUpConclusieStatus = followUpConclusieStatus;
	}

	public void setFollowUpConclusieStatusGewijzigdOp(Date followUpConclusieStatusGewijzigdOp)
	{
		this.followUpConclusieStatusGewijzigdOp = followUpConclusieStatusGewijzigdOp;
	}

	public Date getFollowUpConclusieStatusGewijzigdOp()
	{
		return followUpConclusieStatusGewijzigdOp;
	}

	public List<MammaFollowUpRadiologieVerslag> getFollowUpRadiologieVerslagen()
	{
		return followUpRadiologieVerslagen;
	}

	public void setFollowUpRadiologieVerslagen(List<MammaFollowUpRadiologieVerslag> followUpRadiologieVerslagen)
	{
		this.followUpRadiologieVerslagen = followUpRadiologieVerslagen;
	}

	public List<MammaFollowUpVerslag> getFollowUpVerslagen()
	{
		return followUpVerslagen;
	}

	public void setFollowUpVerslagen(List<MammaFollowUpVerslag> followUpVerslagen)
	{
		this.followUpVerslagen = followUpVerslagen;
	}

	public Date getLaatstGebeldFollowUpNietGedownload()
	{
		return laatstGebeldFollowUpNietGedownload;
	}

	public void setLaatstGebeldFollowUpNietGedownload(Date laatstGebeldFollowUpNietGedownload)
	{
		this.laatstGebeldFollowUpNietGedownload = laatstGebeldFollowUpNietGedownload;
	}

	public List<MammaUploadBeeldenVerzoek> getUploadBeeldenVerzoeken()
	{
		return uploadBeeldenVerzoeken;
	}

	public void setUploadBeeldenVerzoeken(List<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoeken)
	{
		this.uploadBeeldenVerzoeken = uploadBeeldenVerzoeken;
	}

	public MammaOnderzoek getLaatsteOnderzoek()
	{
		return laatsteOnderzoek;
	}

	public void setLaatsteOnderzoek(MammaOnderzoek laatsteOnderzoek)
	{
		this.laatsteOnderzoek = laatsteOnderzoek;
	}
}
