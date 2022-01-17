/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import {ClientGebeurtenis} from "./ClientGebeurtenis"
import {Huisarts, MammaGeenHuisartsOptie} from "./Huisarts"
import {HuidigeAfspraak} from "./mamma/HuidigeAfspraak"
import {KandidaatAfspraak} from "./mamma/KandidaatAfspraak"
import {BackendObject} from "./BackendObject"

export type MammaDossier = BackendObject & {
    id: number,
    status: string
    aangemeld: boolean;
    laatsteStandplaatsPlaats?: string,
    gebeurtenissenLaatsteRonde: ClientGebeurtenis[],
    huisartsVorigeRonde?: Huisarts,
    huisartsHuidigeRonde?: Huisarts,
    magHuisartsOntkoppelen: boolean,
    geenHuisartsOptieHuidigeRonde?: MammaGeenHuisartsOptie,
    geenHuisartsOptieVorigeRonde?: MammaGeenHuisartsOptie,
    huidigeAfspraak?: HuidigeAfspraak,
    laatsteMammaAfspraakZoekResultaten: KandidaatAfspraak[],
    laatsteMammaStandplaatsPlaatsZoekFilter: string[],
}

export const leegMammaDossier: MammaDossier = {
    id: -1,
    status: "",
    aangemeld: false,
    gebeurtenissenLaatsteRonde: [],
    magHuisartsOntkoppelen: false,
    laatsteMammaAfspraakZoekResultaten: [],
    laatsteMammaStandplaatsPlaatsZoekFilter: [],
    isInSync: false,
}
