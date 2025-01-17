package nl.rivm.screenit.service;

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

import java.util.List;

import javax.annotation.Nonnull;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.ColoscopieCentrumZoekCriteria;
import nl.rivm.screenit.model.enums.ToegangLevel;

public interface OrganisatieZoekService
{
	List<Instelling> zoekOrganisaties(Instelling searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		InstellingGebruiker instellingGebruiker, long first, long count, String sortProperty, boolean asc);

	long countOrganisaties(Instelling searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		InstellingGebruiker instellingGebruiker);

	List<Instelling> getOrganisatiesForNiveau(InstellingGebruiker instellingGebruiker, OrganisatieType organisatieTypeGekozen, ToegangLevel toegangLevel);

	List<Instelling> getAllActieveOrganisatiesWithType(Class<? extends Instelling> instelling);

	List<ColoscopieCentrumWrapper> zoekIntakeLocaties(ColoscopieCentrumZoekCriteria zoekObject, Client client, boolean alleenActiefKamers);

	List<Instelling> getMogelijkeParents(@Nonnull Instelling instelling, @Nonnull InstellingGebruiker loggedInInstellingGebruiker);

	List<Long> getZichtbareInstellingenOpToegangLevel(Instelling instelling, ToegangLevel level, List<OrganisatieType> types);

	List<Instelling> screeningsorganisatiesWaarOrganisatieOndervalt(Instelling organisatie);

	ColoscopieCentrumWrapper getNearestIntakeLocatie(Client client);

	List<Instelling> zoekOrganisatieMetFqdn(String fqdn);
}
