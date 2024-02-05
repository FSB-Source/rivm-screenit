package nl.rivm.screenit.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;

public interface ColonHuisartsService
{

	boolean koppelHuisarts(EnovationHuisarts huisarts, ColonScreeningRonde ronde, Account account);

	boolean ontkoppelHuisarts(ColonScreeningRonde screeningRonde, Account account);

	EnovationHuisarts getActieveHuisartsVanVorigeRonde(ColonScreeningRonde ronde);

	EnovationHuisarts getActieveHuisartsVanRonde(ColonScreeningRonde ronde);

	boolean bevestigVorigeColonHuisarts(Client client, ColonScreeningRonde ronde);

}
