package nl.rivm.screenit.main.service.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectType;

import org.springframework.data.domain.Sort;

public interface ProjectService
{
	void saveOrUpdateProject(Project project, InstellingGebruiker loggedInInstellingGebruiker);

	List<Project> getProjectenVanType(ProjectType type);

	List<Project> getProjecten();

	List<ProjectGroep> getGroepen(ProjectGroep zoekObject, long first, long count, Sort sort);

	long getCountGroepen(ProjectGroep zoekObject);

	List<ProjectClient> getClientProjecten(ProjectClient zoekObject, long first, long count, Sort sort);

	long getCountClientProjecten(ProjectClient zoekObject);

	List<ProjectBriefActie> getProjectBriefActies(ProjectBriefActie zoekObject, long first, long count, Sort sort);

	long getCountProjectBriefActies(ProjectBriefActie zoekObject);

	void queueProjectBestandVoorAttributen(Project project, ProjectGroep groep, ProjectBestand projectBestand, String contentType, String filenaam, File file,
		Account loggedInAccount) throws IOException;

	void queueProjectBestandVoorUitslagen(Project project, ProjectBestand projectBestand, String contentType, String filenaam, File file,
		Account loggedInAccount) throws IOException;

	void queueProjectBestandVoorClientWijzigingen(Project project, ProjectBestand projectBestand, UploadDocument bestand, String contentType, String clientFileName, File file,
		Account loggedInAccount) throws IOException;

	void queueProjectBestandVoorPopulatie(ProjectGroep groep, String contentType, String filenaam, File file, Account loggedInAccount) throws IOException;

	List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type);

	List<ProjectType> getProjectTypes(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo);

	Long getAantalProjectClientenVanProject(Project project);

	Long getAantalInactieveProjectClientenVanProject(Project project);

	Long getAantalInactieveProjectClientenVanProjectGroep(ProjectGroep groep);

	void verwerkProjectBestand(ProjectBestand projectBestand);

	void verwerkProjectUitslagen(ProjectBestand uitslagenBestand);

	void projectAttribuutOpslaan(ProjectAttribuut attribuut);

	void verwijderProjectGroep(ProjectGroep groep, Account loggedInAccount);

	String updateProjectGroepActiefStatus(ProjectGroep groep, Account loggedInAccount);

	List<ProjectClientAttribuut> getAttributenVoorProjectClient(ProjectClient zoekObject, long first, long count, Sort sort);

	long getAantalAttributenVoorProjectClient(ProjectClient zoekObject);

	List<ProjectBestandVerwerkingEntry> getProjectBestandVerwerkingEntries(ProjectBestandVerwerkingEntry zoekObject, long first, long count, Sort sort);

	long getAantalProjectbestandVerwerkingEntries(ProjectBestandVerwerkingEntry zoekObject);

	List<ProjectAttribuut> getProjectAttributen(ProjectAttribuut zoekObject, long first, long count, Sort sort);

	long getAantalProjectAttributen(ProjectAttribuut zoekObject);

	List<ProjectBestand> getProjectBestanden(ProjectBestand zoekObject, long first, long count, Sort sort);

	long getAantalProjectBestanden(ProjectBestand zoekObject);

	ProjectAttribuut getProjectAttribuut(ProjectAttribuut zoekObject);
}
