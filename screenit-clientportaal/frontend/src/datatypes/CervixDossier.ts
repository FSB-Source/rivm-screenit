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
import {geenZasStatus, ZasStatus} from "./cervix/ZasStatus"
import {CervixUitstelStatus, geenCervixUitstelStatus} from "./cervix/CervixUitstelStatus"
import {CervixUitstel, geenCervixUitstel} from "./cervix/CervixUitstelDto"
import {BackendObject} from "./BackendObject"

export type CervixDossier = BackendObject & {
    id: number,
    status: string
    aangemeld: boolean;
    gebeurtenissenLaatsteRonde: ClientGebeurtenis[],
    zasStatus: ZasStatus,
    uitstelStatus: CervixUitstelStatus,
    uitstel: CervixUitstel,
}

export const leegCervixDossier: CervixDossier = {
    id: -1,
    status: "",
    aangemeld: false,
    gebeurtenissenLaatsteRonde: [],
    zasStatus: geenZasStatus,
    uitstelStatus: geenCervixUitstelStatus,
    uitstel: geenCervixUitstel,
    isInSync: false,
}
