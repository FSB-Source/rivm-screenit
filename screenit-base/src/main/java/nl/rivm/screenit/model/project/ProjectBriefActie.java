
package nl.rivm.screenit.model.project;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen")
@Audited
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ProjectBriefActie extends AbstractHibernateObject implements IActief, IDocument
{

	private static final long serialVersionUID = 1L;

	private Boolean actief = true;

	@Column
	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@ManyToOne(fetch = FetchType.LAZY)
	private Project project;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ProjectBriefActieType type;

	@Column(nullable = true)
	private Integer aantalDagen;

	@Temporal(TemporalType.DATE)
	private Date datum;

	@ManyToOne(optional = false)
	private UploadDocument document;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date laatstGewijzigd;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker uploader;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private ProjectVragenlijst vragenlijst;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private ProjectVragenlijstUitzettenVia projectVragenlijstUitzettenVia;

	@Column(nullable = true)
	private String misluktBak;

	@Transient
	private boolean herinneren = false;

	@OneToOne(fetch = FetchType.LAZY)
	private ProjectBriefActie herinneringsActie;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "herinneringsActie")
	private ProjectBriefActie baseActie;

	private String formulierNummer;

	private String printomschrijving;

	public ProjectBriefActie()
	{
	}

	public ProjectBriefActie(Project project)
	{
		this.project = project;
	}

	public ProjectBriefActieType getType()
	{
		return type;
	}

	public void setType(ProjectBriefActieType type)
	{
		this.type = type;
	}

	public Project getProject()
	{
		return project;
	}

	public void setProject(Project project)
	{
		this.project = project;
	}

	public Integer getAantalDagen()
	{
		return aantalDagen;
	}

	public void setAantalDagen(Integer aantalDagen)
	{
		this.aantalDagen = aantalDagen;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	@Override
	public Boolean getActief()
	{
		return this.actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;

	}

	public BriefType getBriefType()
	{
		return briefType;
	}

	public void setBriefType(BriefType briefType)
	{
		this.briefType = briefType;
	}

	@Override
	public UploadDocument getDocument()
	{
		return document;
	}

	@Override
	public void setDocument(UploadDocument document)
	{
		this.document = document;
	}

	@Override
	public Date getLaatstGewijzigd()
	{
		return laatstGewijzigd;
	}

	@Override
	public void setLaatstGewijzigd(Date laatstGewijzigd)
	{
		this.laatstGewijzigd = laatstGewijzigd;
	}

	public InstellingGebruiker getUploader()
	{
		return uploader;
	}

	public void setUploader(InstellingGebruiker uploader)
	{
		this.uploader = uploader;
	}

	public ProjectVragenlijst getVragenlijst()
	{
		return vragenlijst;
	}

	public void setVragenlijst(ProjectVragenlijst vragenlijst)
	{
		this.vragenlijst = vragenlijst;
	}

	public String getMisluktBak()
	{
		return misluktBak;
	}

	public void setMisluktBak(String misluktBak)
	{
		this.misluktBak = misluktBak;
	}

	public ProjectVragenlijstUitzettenVia getProjectVragenlijstUitzettenVia()
	{
		return projectVragenlijstUitzettenVia;
	}

	public void setProjectVragenlijstUitzettenVia(ProjectVragenlijstUitzettenVia projectVragenlijstUitzettenVia)
	{
		this.projectVragenlijstUitzettenVia = projectVragenlijstUitzettenVia;
	}

	public boolean isHerinneren()
	{
		return herinneren;
	}

	public void setHerinneren(boolean herinneren)
	{
		this.herinneren = herinneren;
	}

	public ProjectBriefActie getHerinneringsActie()
	{
		return herinneringsActie;
	}

	public void setHerinneringsActie(ProjectBriefActie herinneringsActie)
	{
		this.herinneringsActie = herinneringsActie;
	}

	public ProjectBriefActie getBaseActie()
	{
		return baseActie;
	}

	public void setBaseActie(ProjectBriefActie baseActie)
	{
		this.baseActie = baseActie;
	}

	public String getFormulierNummer()
	{
		return formulierNummer;
	}

	public void setFormulierNummer(String formulierNummer)
	{
		this.formulierNummer = formulierNummer;
	}

	public String getPrintomschrijving()
	{
		return printomschrijving;
	}

	public void setPrintomschrijving(String printomschrijving)
	{
		this.printomschrijving = printomschrijving;
	}
}
