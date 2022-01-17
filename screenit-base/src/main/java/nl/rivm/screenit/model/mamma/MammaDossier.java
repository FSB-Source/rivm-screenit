package nl.rivm.screenit.model.mamma;

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
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.util.SkipFieldForDiff;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(
	schema = "mamma",
	name = "dossier",
	indexes = {
		@Index(name = "idx_MAMMA_DOSSIER_STATUS", columnList = "status"),
		@Index(name = "idx_MAMMA_DOSSIER_FU_CONCLUSIE", columnList = "updateFollowUpConclusie"),
	},
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "laatste_screening_ronde"),
		@UniqueConstraint(columnNames = "laatste_afmelding"),
		@UniqueConstraint(columnNames = "screening_ronde_event"),
	})
@Audited
public class MammaDossier extends Dossier<MammaScreeningRonde, MammaAfmelding>
{
	@OneToOne(mappedBy = "mammaDossier", optional = false, fetch = FetchType.LAZY)
	private Client client;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "dossier")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaScreeningRonde> screeningRondes = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaScreeningRonde laatsteScreeningRonde;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaBeoordeling laatsteBeoordelingMetUitslag;

	@Column(nullable = false)
	private Boolean updateFollowUpConclusie = false;

	@OneToMany(mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaAfmelding laatsteAfmelding;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaDoelgroep doelgroep = MammaDoelgroep.REGULIER;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaTehuis tehuis;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date laatsteMammografieAfgerond;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaStandplaatsRonde eersteMammografieAfgerondStandplaatsRonde;

	@Transient
	@SkipFieldForDiff
	private Boolean uitTeNodigen;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	@SkipFieldForDiff
	private String dubbeleTijdReden;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	@SkipFieldForDiff
	private MammaKansberekeningScreeningRondeEvent screeningRondeEvent;

	@OneToOne(mappedBy = "dossier", optional = false, fetch = FetchType.LAZY)
	@NotAudited
	@SkipFieldForDiff
	private MammaDeelnamekans deelnamekans;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private XdsStatus xdsStatus;

	@OneToMany(mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaIlmBezwaarPoging> ilmBezwaarPogingen = new ArrayList<>();

	@Override
	public Client getClient()
	{
		return client;
	}

	@Override
	public void setClient(Client client)
	{
		this.client = client;
	}

	@Override
	public List<MammaScreeningRonde> getScreeningRondes()
	{
		return screeningRondes;
	}

	@Override
	public void setScreeningRondes(List<MammaScreeningRonde> screeningRondes)
	{
		this.screeningRondes = screeningRondes;
	}

	@Override
	public MammaScreeningRonde getLaatsteScreeningRonde()
	{
		return laatsteScreeningRonde;
	}

	@Override
	public void setLaatsteScreeningRonde(MammaScreeningRonde laatsteScreeningRonde)
	{
		this.laatsteScreeningRonde = laatsteScreeningRonde;
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

	public MammaDoelgroep getDoelgroep()
	{
		return doelgroep;
	}

	public void setDoelgroep(MammaDoelgroep doelgroep)
	{
		this.doelgroep = doelgroep;
	}

	public MammaTehuis getTehuis()
	{
		return tehuis;
	}

	public void setTehuis(MammaTehuis tehuis)
	{
		this.tehuis = tehuis;
	}

	@Transient
	public Boolean getUitTeNodigen()
	{
		return uitTeNodigen;
	}

	@Transient
	public void setUitTeNodigen(Boolean uitTeNodigen)
	{
		this.uitTeNodigen = uitTeNodigen;
	}

	public String getDubbeleTijdReden()
	{
		return dubbeleTijdReden;
	}

	public void setDubbeleTijdReden(String dubbeleTijdReden)
	{
		this.dubbeleTijdReden = dubbeleTijdReden;
	}

	public Date getLaatsteMammografieAfgerond()
	{
		return laatsteMammografieAfgerond;
	}

	public void setLaatsteMammografieAfgerond(Date laatsteMammografieAfgerond)
	{
		this.laatsteMammografieAfgerond = laatsteMammografieAfgerond;
	}

	public MammaStandplaatsRonde getEersteMammografieAfgerondStandplaatsRonde()
	{
		return eersteMammografieAfgerondStandplaatsRonde;
	}

	public void setEersteMammografieAfgerondStandplaatsRonde(MammaStandplaatsRonde eersteMammografieAfgerondStandplaatsRonde)
	{
		this.eersteMammografieAfgerondStandplaatsRonde = eersteMammografieAfgerondStandplaatsRonde;
	}

	public MammaKansberekeningScreeningRondeEvent getScreeningRondeEvent()
	{
		return screeningRondeEvent;
	}

	public void setScreeningRondeEvent(MammaKansberekeningScreeningRondeEvent screeningRondeEvent)
	{
		this.screeningRondeEvent = screeningRondeEvent;
	}

	public MammaDeelnamekans getDeelnamekans()
	{
		return deelnamekans;
	}

	public void setDeelnamekans(MammaDeelnamekans deelnamekans)
	{
		this.deelnamekans = deelnamekans;
	}

	public XdsStatus getXdsStatus()
	{
		return xdsStatus;
	}

	public void setXdsStatus(XdsStatus xdsStatus)
	{
		this.xdsStatus = xdsStatus;
	}

	public MammaBeoordeling getLaatsteBeoordelingMetUitslag()
	{
		return laatsteBeoordelingMetUitslag;
	}

	public void setLaatsteBeoordelingMetUitslag(MammaBeoordeling laatsteBeoordelingMetUitslag)
	{
		this.laatsteBeoordelingMetUitslag = laatsteBeoordelingMetUitslag;
	}

	public Boolean getUpdateFollowUpConclusie()
	{
		return updateFollowUpConclusie;
	}

	public void setUpdateFollowUpConclusie(Boolean updateFollowUpConclusie)
	{
		this.updateFollowUpConclusie = updateFollowUpConclusie;
	}

	public void setIlmBezwaarPogingen(List<MammaIlmBezwaarPoging> bezwaarIlmPogingen)
	{
		this.ilmBezwaarPogingen = bezwaarIlmPogingen;
	}

	public List<MammaIlmBezwaarPoging> getIlmBezwaarPogingen()
	{
		return this.ilmBezwaarPogingen;
	}
}
