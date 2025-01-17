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

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;

public interface GemeenteService
{

	void voegGemeenteToe(Gemeente gemeente);

	List<Gemeente> zoekGemeentes(Gemeente zoekObject, long first, long count, String property, boolean ascending);

	long countGemeentes(Gemeente zoekObject);

	Boolean getGesplitsOpPostcode(Gemeente gemeente);

	List<String> getWoonplaatsen(UitnodigingsGebied modelObject);

	void verwijderAlleGebieden(Gemeente gemeente);

	List<Gemeente> getNietOfAanScreeningsOrganisatieGekoppeldGemeentes(ScreeningOrganisatie screeningOrganisatie);

	List<Gemeente> getNietOfAanBMHKLaboratoriumGekoppeldGemeentes(BMHKLaboratorium bmhkLaboratorium);

	boolean magAlleGebiedenVerwijderen(Gemeente gemeente);

}
