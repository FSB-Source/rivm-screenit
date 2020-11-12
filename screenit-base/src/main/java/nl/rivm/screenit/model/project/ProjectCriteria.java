
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

import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ProjectCriteria extends ProjectSelectie
{

	private static final long serialVersionUID = 1L;

	@ManyToMany(cascade = CascadeType.PERSIST)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@JoinTable(schema = "algemeen", name = "project_criteria_screening_organisaties")
	private List<Instelling> screeningOrganisaties;

	@ElementCollection
	@CollectionTable(schema = "algemeen", name = "project_criteria_geboortejaren")
	private List<Integer> geboortejaren;

	@Enumerated(EnumType.STRING)
	private Geslacht geslacht;

	@ManyToMany(cascade = CascadeType.PERSIST)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@JoinTable(schema = "algemeen", name = "project_criteria_postcodes")
	private List<PostcodeGebied> postcodes;

	@ManyToMany(cascade = CascadeType.PERSIST)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@JoinTable(schema = "algemeen", name = "project_criteria_gebieden")
	private List<UitnodigingsGebied> gebieden;

	public List<Instelling> getScreeningOrganisaties()
	{
		return screeningOrganisaties;
	}

	public void setScreeningOrganisaties(List<Instelling> screeningOrganisaties)
	{
		this.screeningOrganisaties = screeningOrganisaties;
	}

	public List<Integer> getGeboortejaren()
	{
		return geboortejaren;
	}

	public void setGeboortejaren(List<Integer> geboortejaren)
	{
		this.geboortejaren = geboortejaren;
	}

	public Geslacht getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(Geslacht geslacht)
	{
		this.geslacht = geslacht;
	}

	public List<PostcodeGebied> getPostcodes()
	{
		return postcodes;
	}

	public void setPostcodes(List<PostcodeGebied> postcodes)
	{
		this.postcodes = postcodes;
	}

	public List<UitnodigingsGebied> getGebieden()
	{
		return gebieden;
	}

	public void setGebieden(List<UitnodigingsGebied> gebieden)
	{
		this.gebieden = gebieden;
	}

}
