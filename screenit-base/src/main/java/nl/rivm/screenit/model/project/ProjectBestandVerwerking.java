package nl.rivm.screenit.model.project;

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
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ProjectBestandVerwerking extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, optional = false)
	@Cascade({ CascadeType.ALL })
	private ProjectBestand projectBestand;

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.EAGER, mappedBy = "verwerking")
	private List<ProjectBestandVerwerkingEntry> meldingen = new ArrayList<>();

	@Column(nullable = false)
	private int regelsVerwerkt = 0;

	@Column(nullable = false)
	private int regelsMislukt = 0;

	@Column(nullable = true)
	private String attributenGevonden;

	@Column(nullable = false)
	private int attributenGewijzigd = 0;

	@Column(nullable = false)
	private int geinactiveerd = 0;

	@Column(nullable = false)
	private int geheractiveerd = 0;

	@Column(nullable = false)
	private int verwijderd = 0;

	public ProjectBestand getProjectBestand()
	{
		return projectBestand;
	}

	public void setProjectBestand(ProjectBestand projectBestand)
	{
		this.projectBestand = projectBestand;
	}

	public List<ProjectBestandVerwerkingEntry> getMeldingen()
	{
		return meldingen;
	}

	public void setMeldingen(List<ProjectBestandVerwerkingEntry> meldingen)
	{
		this.meldingen = meldingen;
	}

	public int getRegelsVerwerkt()
	{
		return regelsVerwerkt;
	}

	public void setRegelsVerwerkt(int regelsVerwerkt)
	{
		this.regelsVerwerkt = regelsVerwerkt;
	}

	public int getRegelsMislukt()
	{
		return regelsMislukt;
	}

	public void setRegelsMislukt(int regelsMislukt)
	{
		this.regelsMislukt = regelsMislukt;
	}

	public String getAttributenGevonden()
	{
		return attributenGevonden;
	}

	public void setAttributenGevonden(String attributenGevonden)
	{
		this.attributenGevonden = attributenGevonden;
	}

	public int getAttributenGewijzigd()
	{
		return attributenGewijzigd;
	}

	public void setAttributenGewijzigd(int attributenGewijzigd)
	{
		this.attributenGewijzigd = attributenGewijzigd;
	}

	public int getGeinactiveerd()
	{
		return geinactiveerd;
	}

	public void setGeinactiveerd(int geinactiveerd)
	{
		this.geinactiveerd = geinactiveerd;
	}

	public int getGeheractiveerd()
	{
		return geheractiveerd;
	}

	public void setGeheractiveerd(int geheractiveerd)
	{
		this.geheractiveerd = geheractiveerd;
	}

	public int getVerwijderd()
	{
		return verwijderd;
	}

	public void setVerwijderd(int verwijderd)
	{
		this.verwijderd = verwijderd;
	}
}
