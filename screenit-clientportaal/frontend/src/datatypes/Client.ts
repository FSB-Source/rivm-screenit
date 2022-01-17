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
import {BezwaarMoment} from "./Bezwaar"
import {Persoon} from "./Persoon"
import {ContactActiesDto} from "./ContactActiesDto"
import {MammaDossier} from "./MammaDossier"
import {CervixDossier, leegCervixDossier} from "./CervixDossier"
import {ColonDossier} from "./ColonDossier"

export type Client = {
    beschikbareActies: ContactActiesDto,
    cervixDossier: CervixDossier,
    colonDossier: ColonDossier,
    laatsteBezwaarMoment: BezwaarMoment,
    mammaDossier: MammaDossier,
    persoon: Persoon,
    regio: string
}
