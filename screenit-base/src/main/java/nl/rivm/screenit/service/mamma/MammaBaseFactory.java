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

import java.util.Date;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

public interface MammaBaseFactory
{
	MammaScreeningRonde maakRonde(MammaDossier dossier, MammaStandplaatsRonde standplaatsRonde, boolean isGeforceerd);

	MammaUitnodiging maakUitnodiging(MammaScreeningRonde ronde, MammaStandplaatsRonde standplaatsRonde, BriefType briefType);

	MammaAfspraak maakDummyAfspraak(MammaUitnodiging uitnodiging, Date vanaf, MammaCapaciteitBlok capaciteitBlok, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden);

	MammaAfspraak maakAfspraak(MammaScreeningRonde ronde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean notificeerBetrokkenSe, boolean stuurBerichtNaarSectra, boolean isGeforceerdeAfspraak);

	MammaUitstel maakUitstel(MammaScreeningRonde screeningRonde, MammaStandplaats standplaats, Date streefDatum, MammaUitstelReden uitstelReden);

	Long getNextUniqueMammaUitnodigingsNr();

	MammaAdhocMeekijkverzoek maakAdhocMeekijkverzoek(MammaOnderzoek onderzoek, String reden);

	MammaMammografie maakMammografie(MammaOnderzoek onderzoek, InstellingGebruiker instellingGebruiker, MammaAnnotatieAfbeelding afbeelding);
}
