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

import java.io.IOException;
import java.io.OutputStream;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;

public interface OverdrachtPersoonsgegevensService
{

	boolean heeftVerzoekZonderGegenereerdeBrief(Client client);

	void slaOntvangenFormulierOp(OverdrachtPersoonsgegevens overdracht, UploadDocument uploadDocument, Account account) throws IOException;

	void verstuurGeenHandtekeningBrief(OverdrachtPersoonsgegevens overdracht, Account loggedInAccount);

	void afronden(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens);

	void createDataDump(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens, OutputStream outputStream, Account account);
}
