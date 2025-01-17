package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.UploadDocument;

public interface BaseAfmeldService
{
	void definitieveAfmeldingAanvragen(Client client, Afmelding<?, ?, ?> afmelding, boolean rappelBrief, Account account);

	void tijdelijkeAfmeldingAanvragen(Client client, Afmelding<?, ?, ?> afmelding, boolean rappelBrief, Account account);

	void afmelden(Client client, Afmelding<?, ?, ?> afmelding, Account account);

	void afmeldenZonderVervolg(Client client, Afmelding<?, ?, ?> afmelding, boolean handtekeningDocumentVerplicht, Account account);

	<A extends Afmelding<?, ?, ?>> void heraanmelden(A herAanTeMeldenAfmelding, Account account);

	void heraanmeldenZonderVervolg(Afmelding<?, ?, ?> herAanTeMeldenAfmelding);

	boolean vervangAfmeldingDocument(UploadDocument nieuwDocument, Afmelding<?, ?, ?> afmelding, UploadDocument huidigDocument, ClientBrief<?, ?, ?> brief, Account account);

	boolean vervangHeraanmeldingDocument(UploadDocument nieuwDocument, Afmelding<?, ?, ?> afmelding, UploadDocument huidigDocument, ClientBrief<?, ?, ?> brief, Account account);

	void heraanmeldenAlsClientAfgemeldIs(Dossier<?, ?> dossier);
}
