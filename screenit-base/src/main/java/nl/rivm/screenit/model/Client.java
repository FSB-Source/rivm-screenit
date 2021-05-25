package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderBy;
import javax.persistence.Table;
import javax.persistence.Transient;

import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Patient;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.planning.model.IParticipant;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(indexes = { @Index(name = "IDX_GBASTATUS", columnList = "gbaStatus") })
@Audited
public class Client extends Patient<GbaPersoon> implements Account, IParticipant
{
	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	private ColonDossier colonDossier;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	private CervixDossier cervixDossier;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	private MammaDossier mammaDossier;

	@OneToMany(mappedBy = "client", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<Afspraak> afspraken = new ArrayList<>();

	@OneToMany(mappedBy = "client", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<Complicatie> complicaties = new ArrayList<>();

	@Enumerated(EnumType.STRING)
	private GbaStatus gbaStatus;

	@OneToMany(cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	@JoinTable(schema = "gedeeld", name = "pat_patient_gba_mutaties", joinColumns = { @JoinColumn(name = "pat_patient") })
	private List<GbaMutatie> gbaMutaties = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<GbaVraag> gbaVragen = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ClientContact> contacten = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	@OrderBy("bezwaarDatum DESC")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<BezwaarMoment> bezwaarMomenten = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private BezwaarMoment laatstVoltooideBezwaarMoment;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "client")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<HuisartsBericht> huisartsBerichten = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	@JoinTable(schema = "gedeeld", name = "pat_patient_documents", joinColumns = { @JoinColumn(name = "pat_patient") })
	private List<UploadDocument> documents;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "client")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<ProjectClient> projecten = new ArrayList<ProjectClient>();

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	private GbaMutatie laatsteGbaMutatie;

	@Deprecated
	private Date datumLaatsteUitnodiging;

	@OneToMany(mappedBy = "client")
	@OrderBy("statusDatum DESC")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<OverdrachtPersoonsgegevens> overdrachtPersoonsgegevensLijst = new ArrayList<>();

	public ColonDossier getColonDossier()
	{
		return colonDossier;
	}

	public void setColonDossier(ColonDossier colonDossier)
	{
		this.colonDossier = colonDossier;
	}

	@Override
	public void setId(Long id)
	{
		super.setId(id);
	}

	@Override
	public Long getId()
	{
		return (Long) super.getId();
	}

	@Override
	public String getName()
	{
		return NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(this);
	}

	@Deprecated
	@Override
	public void setName(String name)
	{

	}

	@Override
	public String getDescription()
	{
		return "Client " + getPersoon().getNaamVolledigMetVoornaam();
	}

	@Deprecated
	@Override
	public void setDescription(String description)
	{

	}

	@Override
	@Transient
	public List<? extends IAppointment> getAppointments(Date startTime, Date endTime)
	{
		List<Afspraak> result = new ArrayList<>();

		for (Afspraak afspraak : getAfspraken())
		{
			if (afspraak.getStartTime().after(startTime) && afspraak.getStartTime().before(endTime))
			{
				result.add(afspraak);
			}
		}

		return result;
	}

	public List<Afspraak> getAfspraken()
	{
		return afspraken;
	}

	public void setAfspraken(List<Afspraak> afspraken)
	{
		this.afspraken = afspraken;
	}

	public GbaStatus getGbaStatus()
	{
		return gbaStatus;
	}

	public void setGbaStatus(GbaStatus gbaStatus)
	{
		this.gbaStatus = gbaStatus;
	}

	public List<GbaMutatie> getGbaMutaties()
	{
		return gbaMutaties;
	}

	public void setGbaMutaties(List<GbaMutatie> gbaMutaties)
	{
		this.gbaMutaties = gbaMutaties;
	}

	public List<GbaVraag> getGbaVragen()
	{
		return gbaVragen;
	}

	public void setGbaVragen(List<GbaVraag> gbaVragen)
	{
		this.gbaVragen = gbaVragen;
	}

	public List<ClientContact> getContacten()
	{
		return contacten;
	}

	public void setContacten(List<ClientContact> contacten)
	{
		this.contacten = contacten;
	}

	public List<BezwaarMoment> getBezwaarMomenten()
	{
		return bezwaarMomenten;
	}

	public void setBezwaarMomenten(List<BezwaarMoment> bezwaren)
	{
		this.bezwaarMomenten = bezwaren;
	}

	@Deprecated
	public Date getDatumLaatsteUitnodiging()
	{
		return datumLaatsteUitnodiging;
	}

	@Deprecated
	public void setDatumLaatsteUitnodiging(Date datumLaatsteUitnodiging)
	{
		this.datumLaatsteUitnodiging = datumLaatsteUitnodiging;
	}

	public List<HuisartsBericht> getHuisartsBerichten()
	{
		return huisartsBerichten;
	}

	public void setHuisartsBerichten(List<HuisartsBericht> huisartsBerichten)
	{
		this.huisartsBerichten = huisartsBerichten;
	}

	public List<Complicatie> getComplicaties()
	{
		return complicaties;
	}

	public void setComplicaties(List<Complicatie> complicaties)
	{
		this.complicaties = complicaties;
	}

	public List<UploadDocument> getDocuments()
	{
		return documents;
	}

	public void setDocuments(List<UploadDocument> documents)
	{
		this.documents = documents;
	}

	public List<ProjectClient> getProjecten()
	{
		return projecten;
	}

	public void setProjecten(List<ProjectClient> projecten)
	{
		this.projecten = projecten;
	}

	public CervixDossier getCervixDossier()
	{
		return cervixDossier;
	}

	public void setCervixDossier(CervixDossier cervixDossier)
	{
		this.cervixDossier = cervixDossier;
	}

	public MammaDossier getMammaDossier()
	{
		return mammaDossier;
	}

	public void setMammaDossier(MammaDossier mammaDossier)
	{
		this.mammaDossier = mammaDossier;
	}

	public BezwaarMoment getLaatstVoltooideBezwaarMoment()
	{
		return laatstVoltooideBezwaarMoment;
	}

	public void setLaatstVoltooideBezwaarMoment(BezwaarMoment laatstVoltooideBezwaarMoment)
	{
		this.laatstVoltooideBezwaarMoment = laatstVoltooideBezwaarMoment;
	}

	public GbaMutatie getLaatsteGbaMutatie()
	{
		return laatsteGbaMutatie;
	}

	public void setLaatsteGbaMutatie(GbaMutatie laatsteGbaMutatie)
	{
		this.laatsteGbaMutatie = laatsteGbaMutatie;
	}

	public List<OverdrachtPersoonsgegevens> getOverdrachtPersoonsgegevensLijst()
	{
		return overdrachtPersoonsgegevensLijst;
	}

	public void setOverdrachtPersoonsgegevensLijst(List<OverdrachtPersoonsgegevens> overdrachtPersoonsgegevensLijst)
	{
		this.overdrachtPersoonsgegevensLijst = overdrachtPersoonsgegevensLijst;
	}
}
