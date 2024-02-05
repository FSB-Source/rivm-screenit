package nl.rivm.screenit.clientportaal.services;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.model.AfmeldOptiesDto;
import nl.rivm.screenit.clientportaal.model.AfmeldingDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;

public interface AfmeldenService
{
    AfmeldOptiesDto getAfmeldOpties(Client client, Bevolkingsonderzoek bvo);

    CervixAfmelding valideerEnGetCervixAfmelding(AfmeldingDto<CervixAfmeldingReden> afmeldingDto, Client client);

    ColonAfmelding valideerEnGetColonAfmelding(AfmeldingDto<ColonAfmeldingReden> afmeldingDto, Client client);

    MammaAfmelding valideerEnGetMammaAfmelding(AfmeldingDto<MammaAfmeldingReden> afmeldingDto, Client client);
}
