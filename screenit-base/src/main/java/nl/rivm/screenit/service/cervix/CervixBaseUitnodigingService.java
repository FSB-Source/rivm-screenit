
package nl.rivm.screenit.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

public interface CervixBaseUitnodigingService
{

	void saveMonster(CervixZas zas, InstellingGebruiker loggedInAccount, String logMessage);

	void saveMonster(CervixUitstrijkje uitstrijkje, InstellingGebruiker loggedInInstellingGebruiker, String logMessage);

	void registreerMonsterBarcodeAfgedrukt(CervixMonster monster, InstellingGebruiker loggedInInstellingGebruiker, LogGebeurtenis logGebeurtenis);

	void verwijderResultatenMonster(CervixMonster monster, UploadDocument uploadDocument, InstellingGebruiker loggedInInstellingGebruiker);

	CervixMonster getUitnodigingMagVerwijderdWorden(CervixScreeningRonde screeningRonde);

	void vervangVerwijderdDocument(CervixMonster monster, UploadDocument uploadDocument);
}
