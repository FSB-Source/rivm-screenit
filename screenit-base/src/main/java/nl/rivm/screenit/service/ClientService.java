package nl.rivm.screenit.service;

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

import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.RedenOpnieuwAanvragenClientgegevens;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;

public interface ClientService
{
	Client getClientByBsn(String bsn);

	Client getClientZonderBezwaar(String bsn);

	Client getLaatstAfgevoerdeClient(String bsn);

	Client setTelefoonnummer(Client client);

	Client getClientByBsnFromNg01Bericht(String bsn, String anummer);

	String getVoorNg01EenNieuweBsn(String bsn);

	List<Client> getClientenMetTitel(String titelCode);

	List<Client> zoekClienten(Client zoekObject);

	void saveOrUpdateClient(Client client);

	Dossier getDossier(Client client, Bevolkingsonderzoek bevolkingsonderzoek);

	boolean isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(UploadDocument handtekeningBrief, String handtekeningProperty);

	Client getClientByAnummer(String stringUitBericht);

	boolean heeftClientIntakeConclusieMetBezwaar(String bsn);

	void deleteDocumentForClient(UploadDocument modelObject, Client object);

	void saveDocumentForClient(UploadDocument nieuwDocument, Client nullSafeGet);

	void actiesNaUpdateWithGba(Client client);

	void alleProjectClientenInactiveren(Client client, ProjectInactiefReden projectInactiefReden, Bevolkingsonderzoek bvo);

	void projectClientInactiveren(Client client, ProjectInactiefReden reden, Bevolkingsonderzoek bevolkingsonderzoek);

	void projectClientInactiveren(ProjectClient projectClient, ProjectInactiefReden reden, Bevolkingsonderzoek bevolkingsonderzoek);

	String projectClientActiveren(ProjectClient projectClient);

	boolean isClientOverleden(Client client);

	boolean clientInBuitenland(Client client);

	GbaVraag vraagGbaGegevensOpnieuwAan(Client client, Account account, RedenOpnieuwAanvragenClientgegevens reden);

	boolean isTijdelijkeAdresNuActueel(GbaPersoon persoon);

	void saveOrUpdateTijdelijkGbaAdres(Client client, InstellingGebruiker ingelogdeGebruiker);

	void verwijderTijdelijkGbaAdres(Client client, InstellingGebruiker loggedInInstellingGebruiker);

	List<Instelling> getScreeningOrganisatieVan(Client client);

	CervixUitnodiging getLaatstVerstuurdeUitnodiging(CervixScreeningRonde ronde, boolean inclusiefZas);

	boolean heeftDossierMetRondeOfAfmelding(Client client);

	Client getClientMetBriefkenmerk(String briefkenmerk);

	String valideerBriefkenmerk(String briefkenmerk, Client zoekClient);

	CentraleEenheid bepaalCe(Client client);

	String getGbaPostcode(Client client);
}
