/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import type {AnnotatieIcoonType} from '../datatypes/AnnotatieIcoon';
import type {AfspraakDto} from '../datatypes/Afspraak';
import {getNextIcoonId} from '../datatypes/AnnotatieAfbeelding';

export type SignalerenActions = VulSignaleringByAfspraakIdAction | UpdateHeeftAfwijkingenAction | MaakSignaleringIcoonRechtsHorizontaalAction |
    MaakSignaleringIcoonLinksHorizontaalAction |
    CreateActionMaakSignaleringIcoonRechtsVerticaal |
    CreateActionMaakSignaleringIcoonLinksVerticaal |
    SetSignaleringIcoonPositionRechtsVerticaalAction |
    SetSignaleringIcoonPositionLinksVerticaalAction |
    SetSignaleringIcoonPositionRechtsHorizontaalAction |
    SetSignaleringIcoonPositionLinksHorizontaalAction |
    VerwijderSignaleringIcoonRechtsVerticaalAction |
    VerwijderSignaleringIcoonLinksVerticaalAction |
    VerwijderSignaleringIcoonRechtsHorizontaalAction |
    VerwijderSignaleringIcoonLinksHorizontaalAction |
    CreateActionSetSignalering

export const VUL_SIGNALERING_BY_AFSPRAAK_ID = 'VUL_SIGNALERING_BY_AFSPRAAK_ID';
export type VulSignaleringByAfspraakIdAction = { type: 'VUL_SIGNALERING_BY_AFSPRAAK_ID', afspraken: Array<AfspraakDto> };
export const createActionVulSignaleringByAfspraakId = (afspraken: Array<AfspraakDto>): VulSignaleringByAfspraakIdAction => ({
    type: VUL_SIGNALERING_BY_AFSPRAAK_ID,
    afspraken: afspraken,
});

export const UPDATE_HEEFT_AFWIJKINGEN = 'UPDATE_HEEFT_AFWIJKINGEN';
export type UpdateHeeftAfwijkingenAction = { type: 'UPDATE_HEEFT_AFWIJKINGEN', afspraakId: number, heeftAfwijkingen: boolean };
export const createActionUpdateHeeftAfwijkingen = (afspraakId: number, heeftAfwijkingen: boolean): UpdateHeeftAfwijkingenAction => ({
    type: UPDATE_HEEFT_AFWIJKINGEN,
    afspraakId: afspraakId,
    heeftAfwijkingen: heeftAfwijkingen,
});

export const SET_SIGNALERING = 'SET_SIGNALERING';
export type CreateActionSetSignalering = { type: 'SET_SIGNALERING', afspraakId: number, signaleringDto: any }
export const createActionSetSignalering = (afspraakId: number, signaleringDto: any): CreateActionSetSignalering => ({
    type: SET_SIGNALERING,
    afspraakId: afspraakId,
    signaleringDto: signaleringDto,
});

export const MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL = 'MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL';
export type MaakSignaleringIcoonRechtsHorizontaalAction = { type: 'MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL', afspraakId: number, icoonId: number, icoonType: AnnotatieIcoonType, x: number, y: number }
export const createActionMaakSignaleringIcoonRechtsHorizontaal = (
    afspraakId: number, icoonType: AnnotatieIcoonType, x: number, y: number): MaakSignaleringIcoonRechtsHorizontaalAction => ({
    ...{type: MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL}, ...maakSignaleringIcoon(afspraakId, icoonType, x, y),
});

export const MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL = 'MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL';
export type MaakSignaleringIcoonLinksHorizontaalAction = { type: 'MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL', afspraakId: number, icoonId: number, icoonType: AnnotatieIcoonType, x: number, y: number }
export const createActionMaakSignaleringIcoonLinksHorizontaal = (
    afspraakId: number, icoonType: AnnotatieIcoonType, x: number, y: number): MaakSignaleringIcoonLinksHorizontaalAction => ({
    ...{type: MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL}, ...maakSignaleringIcoon(afspraakId, icoonType, x, y),
});

export const MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL = 'MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL';
export type CreateActionMaakSignaleringIcoonRechtsVerticaal = { type: 'MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL', afspraakId: number, icoonId: number, icoonType: AnnotatieIcoonType, x: number, y: number }
export const createActionMaakSignaleringIcoonRechtsVerticaal = (
    afspraakId: number, icoonType: AnnotatieIcoonType, x: number, y: number): CreateActionMaakSignaleringIcoonRechtsVerticaal => ({
    ...{type: MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL}, ...maakSignaleringIcoon(afspraakId, icoonType, x, y),
});

export const MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL = 'MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL';
export type CreateActionMaakSignaleringIcoonLinksVerticaal = { type: 'MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL', afspraakId: number, icoonId: number, icoonType: AnnotatieIcoonType, x: number, y: number }
export const createActionMaakSignaleringIcoonLinksVerticaal = (
    afspraakId: number, icoonType: AnnotatieIcoonType, x: number, y: number): CreateActionMaakSignaleringIcoonLinksVerticaal => ({
    ...{type: MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL}, ...maakSignaleringIcoon(afspraakId, icoonType, x, y),
});

export const SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL = 'SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL';
export type SetSignaleringIcoonPositionRechtsVerticaalAction = { type: 'SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL', afspraakId: number, icoonId: number, x: number, y: number };
export const createActionSetSignaleringIcoonPositionRechtsVerticaal = (
    afspraakId: number, icoonId: number, x: number, y: number): SetSignaleringIcoonPositionRechtsVerticaalAction => ({
    ...{type: SET_SIGNALERING_ICOON_POSITION_RECHTS_VERTICAAL}, ...setSignaleringIcoonPosition(afspraakId, icoonId, x, y),
});

export const SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL = 'SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL ';
export type SetSignaleringIcoonPositionLinksVerticaalAction = { type: 'SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL ', afspraakId: number, icoonId: number, x: number, y: number };
export const createActionSetSignaleringIcoonPositionLinksVerticaal = (
    afspraakId: number, icoonId: number, x: number, y: number): SetSignaleringIcoonPositionLinksVerticaalAction => ({
    ...{type: SET_SIGNALERING_ICOON_POSITION_LINKS_VERTICAAL}, ...setSignaleringIcoonPosition(afspraakId, icoonId, x, y),
});

export const SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL = 'SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL';
export type SetSignaleringIcoonPositionRechtsHorizontaalAction = { type: 'SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL', afspraakId: number, icoonId: number, x: number, y: number };
export const createActionSetSignaleringIcoonPositionRechtsHorizontaal = (
    afspraakId: number, icoonId: number, x: number, y: number): SetSignaleringIcoonPositionRechtsHorizontaalAction => ({
    ...{type: SET_SIGNALERING_ICOON_POSITION_RECHTS_HORIZONTAAL}, ...setSignaleringIcoonPosition(afspraakId, icoonId, x, y),
});

export const SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL = 'SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL';
export type SetSignaleringIcoonPositionLinksHorizontaalAction = { type: 'SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL', afspraakId: number, icoonId: number, x: number, y: number };
export const createActionSetSignaleringIcoonPositionLinksHorizontaal = (
    afspraakId: number, icoonId: number, x: number, y: number): SetSignaleringIcoonPositionLinksHorizontaalAction => ({
    ...{type: SET_SIGNALERING_ICOON_POSITION_LINKS_HORIZONTAAL}, ...setSignaleringIcoonPosition(afspraakId, icoonId, x, y),
});

export const VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL = 'VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL';
export type VerwijderSignaleringIcoonRechtsVerticaalAction = { type: 'VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL', afspraakId: number, icoonId: number };
export const createActionVerwijderSignaleringIcoonRechtsVerticaal = (afspraakId: number, icoonId: number): VerwijderSignaleringIcoonRechtsVerticaalAction => ({
    type: VERWIJDER_SIGNALERING_ICOON_RECHTS_VERTICAAL,
    afspraakId: afspraakId,
    icoonId: icoonId,
});

export const VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL = 'VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL';
export type VerwijderSignaleringIcoonLinksVerticaalAction = { type: 'VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL', afspraakId: number, icoonId: number };
export const createActionVerwijderSignaleringIcoonLinksVerticaal = (afspraakId: number, icoonId: number): VerwijderSignaleringIcoonLinksVerticaalAction => ({
    type: VERWIJDER_SIGNALERING_ICOON_LINKS_VERTICAAL,
    afspraakId: afspraakId,
    icoonId: icoonId,
});

export const VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL = 'VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL';
export type VerwijderSignaleringIcoonRechtsHorizontaalAction = { type: 'VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL', afspraakId: number, icoonId: number };
export const createActionVerwijderSignaleringIcoonRechtsHorizontaal = (afspraakId: number, icoonId: number): VerwijderSignaleringIcoonRechtsHorizontaalAction => ({
    type: VERWIJDER_SIGNALERING_ICOON_RECHTS_HORIZONTAAL,
    afspraakId: afspraakId,
    icoonId: icoonId,
});

export const VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL = 'VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL';
export type VerwijderSignaleringIcoonLinksHorizontaalAction = { type: 'VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL', afspraakId: number, icoonId: number };
export const createActionVerwijderSignaleringIcoonLinksHorizontaal = (afspraakId: number, icoonId: number): VerwijderSignaleringIcoonLinksHorizontaalAction => ({
    type: VERWIJDER_SIGNALERING_ICOON_LINKS_HORIZONTAAL,
    afspraakId: afspraakId,
    icoonId: icoonId,
});

const setSignaleringIcoonPosition = (afspraakId: number, icoonId: number, x: number, y: number) => ({
    afspraakId: afspraakId,
    icoonId: icoonId,
    x: x,
    y: y,
});
const maakSignaleringIcoon = (afspraakId: number, icoonType: AnnotatieIcoonType, x: number, y: number) => ({
    afspraakId: afspraakId,
    icoonId: getNextIcoonId(),
    icoonType: icoonType,
    x: x,
    y: y,
});
