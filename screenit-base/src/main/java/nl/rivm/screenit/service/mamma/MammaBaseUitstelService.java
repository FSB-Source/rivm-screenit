package nl.rivm.screenit.service.mamma;

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

import java.time.LocalDate;
import java.util.Date;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;

public interface MammaBaseUitstelService
{

	void saveUitstel(MammaUitstel uitstel, boolean briefAanmaken, Account account);

    MammaUitstel getOfMaakMammaUitstel(MammaScreeningRonde screeningRonde, MammaStandplaats standplaats, Date zoekDatum);

    void uitstelAfzeggen(MammaUitstel uitstel, MammaUitstelGeannuleerdReden uitstelGeannuleerdReden, Date geannuleerdOp);

    String valideerStandplaatsPeriode(MammaStandplaatsPeriode standplaatsPeriode, LocalDate streefdatum);
}
