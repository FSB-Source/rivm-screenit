package nl.rivm.screenit.service.cervix;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;

public interface CervixHuisartsBerichtService
{
	void sendKlantnummerVerificatieMail(CervixHuisarts huisarts, CervixHuisartsLocatie locatie);

	CervixHuisartsBericht maakCervixHuisartsBericht(HuisartsBerichtType berichtType, Client client, CervixUitstrijkje uitstrijkje, CervixOmissieType omissieType);

	CervixHuisartsBericht maakCervixHuisartsBericht(HuisartsBerichtType berichtType, Client client, CervixHuisartsLocatie huisartsLocatie, CervixUitnodiging uitnodiging);
}
