package nl.rivm.screenit.service.mamma;

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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraakReservering;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;

public interface MammaAfspraakReserveringService
{
	MammaAfspraakReservering maakAfspraakReservering(MammaAfspraak afspraak, InstellingGebruiker gebruiker);

	List<MammaAfspraakReservering> getActieveReserveringenVoorCapaciteitBlok(MammaCapaciteitBlok blok);

	void verwijderReserveringenVoorClient(Client client);

	void verwijderReserveringenVanMedewerker(InstellingGebruiker medewerker);

	void verwijderAfspraakReserveringenDieGemaaktZijnVoor(LocalDateTime moment);
}
