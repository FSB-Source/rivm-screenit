package nl.rivm.screenit.model.project;

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

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ProjectParameter;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen", uniqueConstraints = @UniqueConstraint(name = "uc_project_naam", columnNames = "naam"))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class Project extends AbstractHibernateObject implements INaam, IBevolkingsonderzoek
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private String naam;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date startDatum;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date eindDatum;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date eindeInstroom;

	@ElementCollection(fetch = FetchType.LAZY, targetClass = Bevolkingsonderzoek.class)
	@Enumerated(EnumType.STRING)
	@NotAudited
	@CollectionTable(schema = "algemeen", name = "project_bevolkingsonderzoeken")
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	@Enumerated(EnumType.STRING)
	private GroepSelectieType groepSelectieType;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY)
	private Instelling organisatie;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker contactpersoon;

	@NotAudited
	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "algemeen", name = "project_medewerkers")
	private List<InstellingGebruiker> medewerkers;

	@NotAudited
	@ManyToMany(fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@JoinTable(schema = "algemeen", name = "project_screening_organisaties")
	private List<Instelling> screeningOrganisaties = new ArrayList<Instelling>();

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project")
	private List<ProjectClient> clienten;

	private Boolean excludeerBezwaar = Boolean.FALSE;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "project_excludeer_afmelding")
	private List<Bevolkingsonderzoek> ExcludeerAfmelding = new ArrayList<Bevolkingsonderzoek>();

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "project_excludeer_open_ronde")
	private List<Bevolkingsonderzoek> ExcludeerOpenRonde = new ArrayList<Bevolkingsonderzoek>();

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ProjectGroep> groepen;

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project")
	private List<ProjectBriefActie> projectBriefActies = new ArrayList<ProjectBriefActie>();

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project")
	private List<ProjectAttribuut> projectAttributen = new ArrayList<ProjectAttribuut>();

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project", orphanRemoval = true)
	private List<ProjectParameter> parameters = new ArrayList<>();

	private Boolean anoniem;

	@Column(length = HibernateMagicNumber.L255)
	private String opmerkingen;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private ProjectType type;

	@Transient
	private List<ProjectType> projectTypes = new ArrayList<>();

	@Transient
	private List<ProjectStatus> projectStatussen = new ArrayList<ProjectStatus>();

	@Override
	public String getNaam()
	{
		return naam;
	}

	public Date getStartDatum()
	{
		return startDatum;
	}

	public void setStartDatum(Date startDatum)
	{
		this.startDatum = startDatum;
	}

	public GroepSelectieType getGroepSelectieType()
	{
		return groepSelectieType;
	}

	public void setGroepSelectieType(GroepSelectieType groepSelectieType)
	{
		this.groepSelectieType = groepSelectieType;
	}

	public List<ProjectGroep> getGroepen()
	{
		return groepen;
	}

	public void setGroepen(List<ProjectGroep> groepen)
	{
		this.groepen = groepen;
	}

	public Instelling getOrganisatie()
	{
		return organisatie;
	}

	public void setOrganisatie(Instelling organisatie)
	{
		this.organisatie = organisatie;
	}

	public List<Instelling> getScreeningOrganisaties()
	{
		return screeningOrganisaties;
	}

	public void setScreeningOrganisaties(List<Instelling> screeningOrganisaties)
	{
		this.screeningOrganisaties = screeningOrganisaties;
	}

	public List<ProjectClient> getClienten()
	{
		return clienten;
	}

	public void setClienten(List<ProjectClient> clienten)
	{
		this.clienten = clienten;
	}

	public Boolean getAnoniem()
	{
		return anoniem;
	}

	public void setAnoniem(Boolean anoniem)
	{
		this.anoniem = anoniem;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public ProjectType getType()
	{
		return type;
	}

	public void setType(ProjectType type)
	{
		this.type = type;
	}

	@Transient
	public List<ProjectType> getProjectTypes()
	{
		return projectTypes;
	}

	@Transient
	public void setProjectTypes(List<ProjectType> projectTypes)
	{
		this.projectTypes = projectTypes;
	}

	public List<ProjectStatus> getProjectStatussen()
	{
		return projectStatussen;
	}

	public void setProjectStatussen(List<ProjectStatus> projectStatussen)
	{
		this.projectStatussen = projectStatussen;
	}

	public InstellingGebruiker getContactpersoon()
	{
		return contactpersoon;
	}

	public void setContactpersoon(InstellingGebruiker contactpersoon)
	{
		this.contactpersoon = contactpersoon;
	}

	public List<InstellingGebruiker> getMedewerkers()
	{
		return medewerkers;
	}

	public void setMedewerkers(List<InstellingGebruiker> medewerkers)
	{
		this.medewerkers = medewerkers;
	}

	public String getOpmerkingen()
	{
		return opmerkingen;
	}

	public void setOpmerkingen(String opmerkingen)
	{
		this.opmerkingen = opmerkingen;
	}

	public List<Bevolkingsonderzoek> getExcludeerAfmelding()
	{
		return ExcludeerAfmelding;
	}

	public void setExcludeerAfmelding(List<Bevolkingsonderzoek> excludeerAfmelding)
	{
		ExcludeerAfmelding = excludeerAfmelding;
	}

	public List<Bevolkingsonderzoek> getExcludeerOpenRonde()
	{
		return ExcludeerOpenRonde;
	}

	public void setExcludeerOpenRonde(List<Bevolkingsonderzoek> excludeerOpenRonde)
	{
		ExcludeerOpenRonde = excludeerOpenRonde;
	}

	public List<ProjectBriefActie> getProjectBriefActies()
	{
		return projectBriefActies;
	}

	public void setProjectBriefActies(List<ProjectBriefActie> projectBriefActies)
	{
		this.projectBriefActies = projectBriefActies;
	}

	public int getPopulatie()
	{
		int populatie = 0;
		if (getGroepen() != null)
		{
			for (ProjectGroep groep : getGroepen())
			{
				populatie += groep.getPopulatie();
			}
		}
		return populatie;
	}

	@Override
	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

	@Override
	public void setBevolkingsonderzoeken(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
	}

	@Override
	public Boolean getExactMatch()
	{

		return null;
	}

	@Override
	public void setExctMatch(Boolean exactMatch)
	{

	}

	public Date getEindDatum()
	{
		return eindDatum;
	}

	public void setEindDatum(Date eindDatum)
	{
		this.eindDatum = eindDatum;
	}

	public Date getEindeInstroom()
	{
		return eindeInstroom;
	}

	public void setEindeInstroom(Date eindeInstroom)
	{
		this.eindeInstroom = eindeInstroom;
	}

	public Boolean getExcludeerBezwaar()
	{
		return excludeerBezwaar;
	}

	public void setExcludeerBezwaar(Boolean excludeerBezwaar)
	{
		this.excludeerBezwaar = excludeerBezwaar;
	}

	public List<ProjectAttribuut> getProjectAttributen()
	{
		return projectAttributen;
	}

	public void setProjectAttributen(List<ProjectAttribuut> projectAttributen)
	{
		this.projectAttributen = projectAttributen;
	}

	public List<ProjectParameter> getParameters()
	{
		return parameters;
	}

	public void setParameters(List<ProjectParameter> parameters)
	{
		this.parameters = parameters;
	}

}
