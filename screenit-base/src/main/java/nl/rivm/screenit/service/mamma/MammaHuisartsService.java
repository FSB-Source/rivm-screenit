package nl.rivm.screenit.service.mamma;

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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;

public interface MammaHuisartsService
{

	boolean ontkoppelHuisarts(MammaScreeningRonde ronde, Account account);

	boolean koppelHuisarts(EnovationHuisarts huisarts, MammaScreeningRonde ronde, Account account);

	EnovationHuisarts getActieveHuisartsVanVorigeRonde(MammaScreeningRonde ronde);

	EnovationHuisarts getActieveHuisartsVanRonde(MammaScreeningRonde ronde);

	boolean magHuisartsVerwijderen(MammaScreeningRonde laatsteRonde);

	MammaGeenHuisartsOption getMammaGeenHuisartsOptieVorigeRonde(MammaScreeningRonde laatsteScreeningRonde);

	MammaGeenHuisartsOption getMammaGeenHuisartsOptieVanRonde(MammaScreeningRonde laatsteScreeningRonde);

	boolean bevestigVorigeMammaHuisartsKeuze(Client client, MammaScreeningRonde ronde);
}
