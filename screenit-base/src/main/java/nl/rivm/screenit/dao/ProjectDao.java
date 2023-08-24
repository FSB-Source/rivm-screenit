
package nl.rivm.screenit.dao;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectGroep;

public interface ProjectDao
{

	List<Project> getProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject, long first, long count, SortState<String> sortState);

	long getCountProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject);

	Iterator<ProjectGroep> getGroepen(ProjectGroep zoekObject, long first, long count, SortState<String> sortState);

	long getCountGroepen(ProjectGroep zoekObject);

	Iterator<ProjectAttribuut> getProjectAttributen(ProjectAttribuut filterObject, long first, long count, SortState<String> sortState);

	Long getAantalProjectAttributen(ProjectAttribuut zoekObject);

	Iterator<ProjectBestand> getProjectBestanden(ProjectBestand filterObject, long first, long count, SortState<String> sortState);

	Long getAantalProjectBestanden(ProjectBestand zoekObject);

	Iterator<ProjectClient> getClientProjecten(ProjectClient client, long first, long count, SortState<String> sortState);

	long getCountClientProjecten(ProjectClient client);

	Iterator<ProjectBriefActie> getProjectBriefActies(ProjectBriefActie actie, long first, long count, SortState<String> sortState);

	long getCountProjectBriefActies(ProjectBriefActie actie);

	List<ProjectClient> getValideClientenVanProject(Project project, ProjectBriefActie definitie);

	List<Long> getActieveActiesVoorBrieven(ScreeningOrganisatie so);

	List<ProjectBrief> getAllProjectBriefForHerinnering(ProjectBriefActie actie, Date verstuurdOp);

	List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type);

	boolean isVragenlijstGekoppeldAanNietBeeindigdProject(Long vragenlijstId);

	boolean isVragenlijstGekoppeldAanProject(Long vragenlijstId);

	Long getAantalProjectClientenVanProject(Project project);

	Long getAantalInactieveProjectClientenVanProject(Project project);

	Long getAantalProjectClientenVanProjectGroep(ProjectGroep groep);

	Long getAantalInactieveProjectClientenVanProjectGroep(ProjectGroep groep);

	ProjectBriefActie getProjectBriefActie(Client client, BriefType briefType);

	List<ProjectGroep> getActieveProjectGroepenVoorUitnodigingDK();

	Long getAantalProjectbestandVerwerkingEntries(ProjectBestandVerwerkingEntry entry);

	Iterator<ProjectBestandVerwerkingEntry> getProjectBestandVerwerkingEntries(ProjectBestandVerwerkingEntry entry, long first, long count, SortState<String> sortState);

	ProjectClientAttribuut getProjectClientAttribuut(ProjectClient client, ProjectAttribuut attribuut);

	Iterator<ProjectClientAttribuut> getAttributenVoorProjectClient(ProjectClient filter, long first, long count, SortState<String> sortState);

	Long getAantalAttributenVoorProjectClient(ProjectClient filter);

	ProjectClient getProjectClient(Client client, ProjectBestand bestand);

	ProjectAttribuut getProjectAttribuut(ProjectAttribuut attribuut);

	void resetWachtOpStartProject(Bevolkingsonderzoek bvo);

	void setNieuwWachtOpStartProject(Bevolkingsonderzoek bvo, Date nu);
}
