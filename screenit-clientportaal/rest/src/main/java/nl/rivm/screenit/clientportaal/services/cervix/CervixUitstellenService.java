package nl.rivm.screenit.clientportaal.services.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.model.cervix.CervixUitstelDto;
import nl.rivm.screenit.clientportaal.model.cervix.CervixUitstellenStatusDto;
import nl.rivm.screenit.model.Client;

import java.time.LocalDate;

public interface CervixUitstellenService
{
	CervixUitstellenStatusDto getUitstelStatus(Client client);

    void valideerDatumBijZwangerschap(LocalDate gekozenDatum);

    void valideerDatumBijAnders(LocalDate gekozenDatum);

    void vraagUitstelAan(Client client, CervixUitstelDto uitstellenDto);

    CervixUitstelDto getHuidigeCervixUitstel(Client client);
}
