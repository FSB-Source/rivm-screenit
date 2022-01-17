package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;

public interface MammaBaseTestTimelineService
{

	MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid);

	MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, boolean stuurHl7Bericht);

	MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, MammaStandplaatsRonde standplaatsRonde,
                                                        boolean stuurHl7Bericht);

	MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, MammaStandplaatsRonde standplaatsRonde,
                                                        boolean stuurHl7Bericht, boolean rekenDossierTerug);

	MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, boolean stuurHl7Bericht, Long uitnodigingsNr);

	MammaUitnodiging nieuweRondeMetOpenUitnodiging(Client client);

	MammaUitnodiging nieuweRondeMetOpenUitnodiging(Client client, boolean stuurHl7Bericht);

	MammaAfspraak maakAfspraak(MammaScreeningRonde ronde, MammaScreeningsEenheid screeningsEenheid);

	MammaAfspraak maakAfspraak(MammaScreeningRonde ronde, MammaScreeningsEenheid screeningsEenheid, boolean stuurHl7Bericht);

}
