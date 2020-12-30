
package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.Map;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.NieuweIfobtResultaat;

public interface ClientContactService
{

	void saveClientContact(ClientContact contact, Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, Account ingelogdeGebruiker);

	List<ClientContactActieType> getAvailableActies(Client client);

	List<ClientContactActieType> getAvailableActies(Client client, boolean viaClientPortaal);

	NieuweIfobtResultaat vraagNieuweIfobtAan(Client client, Account account);

	boolean magNieuweIfobtAanvragen(Client client);

	List<AfmeldingType> getAvailableAfmeldoptiesColon(Client client, boolean viaPortaalGevraagd);

	List<AfmeldingType> getAvailableAfmeldoptiesMamma(Client object, boolean viaPortaalGevraagd);

	List<AfmeldingType> getAvailableAfmeldoptiesCervix(Client client, boolean viaPortaalGevraagd);

	boolean magNieuweUitnodigingAanvragen(Dossier dossier, boolean isHeraanmelding);

	boolean defaultNieuweUitnodigingAanvragen(Dossier dossier);

	boolean heeftOpenIntakeAfspraak(Client client);

	void updateContact(ClientContact contact, InstellingGebruiker loggedInInstellingGebruiker);

	void verwijderContact(ClientContact contact, InstellingGebruiker loggedInInstellingGebruiker);

	boolean heeftOpenMammaAfspraak(Client client);

	boolean magNieuweIntakeAfspraakMakenNaHeraanmelding(ColonDossier colonDossier);

}
