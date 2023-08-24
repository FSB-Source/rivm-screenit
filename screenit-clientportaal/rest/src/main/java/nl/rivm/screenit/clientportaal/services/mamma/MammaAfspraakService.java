package nl.rivm.screenit.clientportaal.services.mamma;

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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakOptieDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakZoekFilterDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaHuidigeAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;

public interface MammaAfspraakService
{
    List<LocalDate> getAlleDatumsMetBeschikbareAfspraken(Client client, String plaats, String afstand);

    MammaAfspraakWijzigenFilterDto toAfspraakFilter(MammaAfspraakZoekFilterDto body, Client client, boolean buitenRegio);

	MammaAfspraak toAfspraak(MammaAfspraakOptieDto kandidaatAfspraakDto, Client client);

	MammaAfspraakOptieDto toMammaKandidaatOptie(MammaKandidaatAfspraakDto kandidaatAfspraakDto, Client client);

    MammaHuidigeAfspraakDto toHuidigeAfspraakDto(MammaAfspraak huidigeAfspraak);
}
