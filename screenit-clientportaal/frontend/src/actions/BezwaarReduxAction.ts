/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {BezwaarMoment} from '../datatypes/Bezwaar';
import {BezwaarType} from "../datatypes/BezwaarType";

export type BezwaarActions = ResetBezwaarMoment | BezwaarToggle;

export const RESET_LAATSTE_BEZWAAR_MOMENT = 'RESET_LAATSTE_BEZWAAR_MOMENT';

export type ResetBezwaarMoment = { type: typeof RESET_LAATSTE_BEZWAAR_MOMENT, bezwaarMoment: BezwaarMoment }

export const setLaatsteBezwaarMomentAction = (bezwaarMoment: BezwaarMoment): ResetBezwaarMoment => ({
    type: RESET_LAATSTE_BEZWAAR_MOMENT,
    bezwaarMoment: bezwaarMoment
});

export const BEZWAAR_TOGGLE = 'BEZWAAR_TOGGLE';

export type BezwaarToggle = { type: typeof BEZWAAR_TOGGLE, bezwaarMoment: BezwaarMoment, bezwaarType: BezwaarType }

export const toggleBezwaarAction = (bezwaarMoment: BezwaarMoment, type: BezwaarType): BezwaarToggle => ({
    type: BEZWAAR_TOGGLE,
    bezwaarMoment: bezwaarMoment,
    bezwaarType: type
});
