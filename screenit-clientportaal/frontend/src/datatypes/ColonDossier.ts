/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import {ClientGebeurtenis} from "./ClientGebeurtenis"
import {Huisarts} from "./Huisarts"
import {FitStatus, geenFitStatus} from "./colon/FitStatus"
import {ColonIntakeAfspraakDto} from "./colon/ColonIntakeAfspraakDto"
import {geenHeraanmeldenOpties, HeraanmeldenOptiesDto} from "./afmelden/HeraanmeldenOptiesDto"
import {BackendObject} from "./BackendObject"

export type ColonDossier = BackendObject & {
    id: number,
    status: string
    aangemeld: boolean;
    gebeurtenissenLaatsteRonde: ClientGebeurtenis[],
    huisartsVorigeRonde?: Huisarts,
    huisartsHuidigeRonde?: Huisarts,
    fitStatus: FitStatus,
    intakeAfspraak?: ColonIntakeAfspraakDto,
    heraanmeldenOpties: HeraanmeldenOptiesDto,
}

export const leegColonDossier: ColonDossier = {
    id: -1,
    status: '',
    aangemeld: false,
    gebeurtenissenLaatsteRonde: [],
    fitStatus: geenFitStatus,
    heraanmeldenOpties: geenHeraanmeldenOpties,
    isInSync: false
}
