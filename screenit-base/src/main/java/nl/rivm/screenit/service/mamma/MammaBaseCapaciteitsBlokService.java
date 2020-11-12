package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Collection;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.mamma.impl.MammaCapaciteit;

public interface MammaBaseCapaciteitsBlokService
{
	String saveOrUpdate(PlanningCapaciteitBlokDto blok, InstellingGebruiker ingelogdeGebruiker);

	String delete(PlanningCapaciteitBlokDto blok, InstellingGebruiker loggedInInstellingGebruiker);

	int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete);

	List<MammaCapaciteitBlok> getCapaciteitsBlokken(MammaScreeningsEenheid screeningsEenheid, Date start, Date end, boolean bepaalCapaciteit,
		Collection<MammaCapaciteitBlokType> blokTypes);

	MammaCapaciteit getCapaciteit(Collection<MammaCapaciteitBlokDto> nietGeblokkeerdeCapaciteitsBlokDtos);

	MammaCapaciteitBlok getCapaciteitsBlokOpTijdstipVoorSe(Client client, MammaScreeningsEenheid screeningsEenheid, Date nu);

	Collection<MammaCapaciteitBlokDto> getNietGeblokkerdeCapaciteitsBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date vanaf, Date totEnMet,
		Collection<MammaCapaciteitBlokType> blokTypes);

}
