package nl.rivm.screenit.dao.mamma;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;

public interface MammaBaseStandplaatsDao
{

	List<MammaStandplaats> zoekStandplaatsen(MammaStandplaats zoekObject, int first, int count, String sortProperty, boolean asc);

	long countStandplaatsen(MammaStandplaats zoekObject);

	List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie voorRegio);

	boolean heeftActieveOpmerking(MammaStandplaats standplaats);

	boolean heeftStandplaatsRondenBijScreeningsRonden(MammaStandplaatsRonde ronde);

	boolean heeftAfspraken(MammaStandplaatsRonde ronde);

	boolean magStandplaatsInactiveren(MammaStandplaats standplaats, Date vandaag);

	MammaStandplaats getStandplaatsMetPostcode(String postcode);

	MammaStandplaatsRonde getVorigeStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde);

	long countActieveStandplaatsPeriodes(MammaStandplaats standplaats);

	List<MammaStandplaatsPeriode> getStandplaatsPerioden(IMammaAfspraakWijzigenFilter filter);
}
