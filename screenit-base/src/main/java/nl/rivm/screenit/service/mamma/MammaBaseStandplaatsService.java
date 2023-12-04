package nl.rivm.screenit.service.mamma;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;

public interface MammaBaseStandplaatsService
{

	List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter);

	List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter,
		boolean validatieUitvoeren);

	List<String> getStandplaatsPlaatsenVanActievePeriodes(IMammaAfspraakWijzigenFilter filter, boolean uitstellen);

	List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(IMammaAfspraakWijzigenFilter filter, boolean uitstellen);

	MammaStandplaats getStandplaatsMetPostcode(Client client);

	MammaStandplaatsPeriode getEerstvolgendeStandplaatsPeriode(MammaStandplaats standplaats);

	Double bepaalAfstand(MammaStandplaats standplaats, Client client);

	MammaStandplaatsLocatie getStandplaatsLocatie(MammaStandplaats standplaats, Date datum);

	void zetBrievenKlaarVoorStandplaatsVoorAfdrukken(List<MammaBrief> brieven, MammaStandplaats standplaats);

	List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie voorRegio);

	Date getMaximaleVrijgegevenTotEnMetDatum(List<MammaStandplaatsPeriode> standplaatsPerioden);

	MammaStandplaatsPeriode huidigeStandplaatsPeriodeInRouteVanStandplaats(MammaStandplaats standplaats);

	boolean isActieveStandplaatsPeriodeVerkort(MammaStandplaatsPeriode persistentStandplaatsPeriode, LocalDate nieuweTotEnMet);
}
