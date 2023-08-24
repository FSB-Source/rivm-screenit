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
import java.util.List;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonUitnodiging;

public interface ClientDao
{

	void saveOrUpdateClient(Client client);

	Client getClientByBsn(String bsn);

	Client getClientZonderBezwaar(String bsn);

	Client getLaatstAfgevoerdeClient(String bsn);

	Client getClientByBsnFromNg01Bericht(String bsn, String anummer);

	Client getClientByANummer(String anummer);

	List<Client> getClientenMetTitel(String titelCode);

	List<Client> zoekClienten(Client zoekObject);

	List<Client> getClientenOpAdresMetLimiet(BagAdres adres, Integer minmaleLeeftijd, Integer maximaleLeeftijd, int uitnodigingsInterval);

	List<ClientContact> getClientContacten(Client client, long first, long count, String sortProperty, boolean ascending);

	List<ClientContact> getClientContacten(Client client);

	boolean heeftClientIntakeConclusieMetBezwaar(String bsn);

	boolean heeftDossierMetRondeOfAfmelding(Client client);

	int countUsedColonHandtekeningBrief(UploadDocument handtekeningDocumentAfmelding, String handtekeningProperty);

	List<ColonUitnodiging> getAllColonUitnodigingenVanClientInPeriode(Client client, Date begin, Date eind);
}
