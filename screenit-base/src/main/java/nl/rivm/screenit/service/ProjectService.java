
package nl.rivm.screenit.service;

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

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectImportMelding;
import nl.rivm.screenit.model.project.ProjectInactiveerDocument;
import nl.rivm.screenit.model.project.ProjectType;

import com.aspose.words.Document;

public interface ProjectService
{
	
	void saveOrUpdateProject(Project project, InstellingGebruiker loggedInInstellingGebruiker);

	List<Project> getProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject, long first, long count, SortState<String> sortState);

	long getCountProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject);

	Iterator<ProjectGroep> getGroepen(ProjectGroep zoekObject, long first, long count, SortState<String> sortState);

	long getCountGroepen(ProjectGroep zoekObject);

	Iterator<ProjectClient> getClientProjecten(ProjectClient client, long first, long count, SortState<String> sortState);

	long getCountClientProjecten(ProjectClient client);

	Map<ProjectImportMelding, List<String>> verwerkXLSNaarProjectClienten(ProjectGroep groep, InstellingGebruiker ingelogdeGebruiker, UploadDocument upload, Boolean skipFouten);

	void queueProjectBestandVoorAttributen(Project project, ProjectGroep groep, ProjectBestand projectBestand, String contentType, String filenaam, File file,
		Account loggedInAccount) throws IOException;

	void queueProjectBestandVoorUitslagen(Project project, ProjectBestand projectBestand, String contentType, String filenaam, File file,
		Account loggedInAccount) throws IOException;

	void queueProjectBestandVoorClientWijzigingen(Project project, ProjectBestand projectBestand, UploadDocument bestand, String contentType, String clientFileName, File file,
		Account loggedInAccount) throws IOException;

	void queueProjectBestandVoorPopulatie(ProjectGroep groep, String contentType, String filenaam, File file, Account loggedInAccount) throws IOException;

	Map<ProjectImportMelding, List<String>> inactiveerProjectClienten(ProjectInactiveerDocument projectInactiveerDocument);

	boolean verwerkGroepClienten(List<Client> clienten, ProjectGroep groep);

	Iterator<ProjectBriefActie> getProjectBriefActies(ProjectBriefActie actie, long first, long count, SortState<String> sortState);

	long getCountProjectBriefActies(ProjectBriefActie actie);

	List<ProjectClient> getValideClientenVanProject(Project project, ProjectBriefActie definitie);

	List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type);

	List<ProjectBrief> getAllProjectBriefForHerinnering(ProjectBriefActie actie, Date verstuurdOp);

	List<ProjectType> getProjectTypes(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo);

	String generateVragenlijstUrl(ProjectBrief projectBrief);

	String generateVragenlijstKey(ProjectBrief projectBrief);

	ProjectBrief getProjectBriefFromVragenlijstKey(String url);

	boolean isVragenlijstGekoppeldAanNietBeeindigdProject(Long vragenlijstId);

	boolean isVragenlijstGekoppeldAanProject(Long vragenlijstId);

	Long getAantalProjectClientenVanProject(Project project);

	Long getAantalInactieveProjectClientenVanProject(Project project);

	Long getAantalProjectClientenVanProjectGroep(ProjectGroep groep);

	Long getAantalInactieveProjectClientenVanProjectGroep(ProjectGroep groep);

	ProjectBriefActie getProjectBriefActie(Client client, BriefType briefType);

	boolean addVragenlijstAanTemplate(MailMergeContext context, Document chunkDocument, ProjectBriefActie actie, ProjectBrief projectBrief) throws Exception;

	List<ProjectGroep> getActieveProjectGroepenVoorUitnodigingDK();

	void verwerkProjectBestand(ProjectBestand projectBestand);

	void verwerkProjectUitslagen(ProjectBestand uitslagenBestand);

	void projectAttribuutOpslaan(ProjectAttribuut attribuut);

	void updateWachtOpStartProject();
}
