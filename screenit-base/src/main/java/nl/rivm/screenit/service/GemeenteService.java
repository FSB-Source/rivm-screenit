package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;

public interface GemeenteService
{

	void voegGemeenteToe(Gemeente gemeente);

	List<Gemeente> zoekGemeentes(Gemeente zoekObject, long first, long count, String property, boolean ascending);

	long countGemeentes(Gemeente nullSafeGet);

	Boolean getGesplitsOpPostcode(Gemeente gemeente);

	List<String> getWoonplaatsen(UitnodigingsGebied modelObject);

	void verwijderAlleGebieden(Gemeente gemeente);

	Iterator<? extends UitnodigingsGebied> getGebieden(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum, long first, long count, String property,
		boolean ascending);

	long getCountGebieden(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum);

	List<Gemeente> getGemeentesZonderScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie);

	List<Gemeente> getGemeentesZonderBMHKLaboratorium(BMHKLaboratorium bmhkLaboratorium);

	boolean magAlleGebiedenVerwijderen(Gemeente gemeente);

	List<Gemeente> getAllGekoppeldeGemeentes();

	Gemeente getGemeenteByCode(String code);
}
