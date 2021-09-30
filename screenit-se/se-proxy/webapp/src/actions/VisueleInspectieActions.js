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

import type {AnnotatieIcoonDto, AnnotatieIcoonType} from '../datatypes/AnnotatieIcoon';
import type {AnnotatieAfbeelding, AnnotatieAfbeeldingDto} from '../datatypes/AnnotatieAfbeelding';
import {getNextIcoonId, mapAfbeeldingToIconenArrayDto} from '../datatypes/AnnotatieAfbeelding';

export type VisueleInspectieActions = MammografieOpslaanAction |
    MammografieWijzigenAction |
    VulVisueleInspectieAfbeeldingenByAfspraakIdAction |
    VulVisueleInspectieAfbeeldingByAfspraakIdAction |
    MaakVisueleInsopectieIcoonAction |
    SetVisueleInspectieIcoonPositionAction |
    SetVisueleInspectieIcoonTekstAction |
    CreateActionSetVisueleInspectieAfbeelding |
    VerwijderVisueleInspectieIcoonAction |
    DeleteVisueleInspectieAfbeeldingByAfspraakId;

export const MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG = 'MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG';
export type MammografieOpslaanAction = { type: 'MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG', afspraakId: number, mammografie: { visueleInspectieAfbeelding: { iconen: Array<AnnotatieIcoonDto> } } };
export const createActionMammografieOpslaan = (afspraakId: number, visueleInspectieAfbeelding: AnnotatieAfbeelding): MammografieOpslaanAction => {
    return {
        type: MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG,
        afspraakId: afspraakId,
        mammografie: {
            visueleInspectieAfbeelding: {
                iconen: mapAfbeeldingToIconenArrayDto(visueleInspectieAfbeelding),
            },
        },
    };
};

export const MAMMOGRAFIE_OPSLAAN = 'MAMMOGRAFIE_OPSLAAN';
export type MammografieWijzigenAction = { type: 'MAMMOGRAFIE_OPSLAAN', afspraakId: number, mammografie: { visueleInspectieAfbeelding: { iconen: Array<AnnotatieIcoonDto> } } };
export const createActionMammografieWijzigen = (afspraakId: number, visueleInspectieAfbeelding: AnnotatieAfbeelding): MammografieWijzigenAction => {
    return {
        type: MAMMOGRAFIE_OPSLAAN,
        afspraakId: afspraakId,
        mammografie: {
            visueleInspectieAfbeelding: {
                iconen: mapAfbeeldingToIconenArrayDto(visueleInspectieAfbeelding),
            },
        },
    };
};

export const SET_VISUELE_INSPECTIE_AFBEELDING = 'SET_VISUELE_INSPECTIE_AFBEELDING';
export type CreateActionSetVisueleInspectieAfbeelding = { type: 'SET_VISUELE_INSPECTIE_AFBEELDING', afspraakId: number, visueleInspectieAfbeeldingDto: AnnotatieAfbeeldingDto };
export const createActionSetVisueleInspectieAfbeelding = (afspraakId: number, visueleInspectieAfbeeldingDto: AnnotatieAfbeeldingDto):
    CreateActionSetVisueleInspectieAfbeelding =>
    ({
        type: SET_VISUELE_INSPECTIE_AFBEELDING,
        afspraakId: afspraakId,
        visueleInspectieAfbeeldingDto: visueleInspectieAfbeeldingDto,
    });

export const VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID = 'VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID';
export type VulVisueleInspectieAfbeeldingenByAfspraakIdAction = { type: 'VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID', visueleInspectieAfbeeldingen: Array<AnnotatieAfbeelding> };
export const createActionVulVisueleInspectieAfbeeldingenByAfspraakId = (visueleInspectieAfbeeldingen: Array<AnnotatieAfbeelding>): VulVisueleInspectieAfbeeldingenByAfspraakIdAction => ({
    type: VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID,
    visueleInspectieAfbeeldingen: visueleInspectieAfbeeldingen,
});

export const DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID = 'DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID';
export type DeleteVisueleInspectieAfbeeldingByAfspraakId = { type: 'DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID', afspraakId: number };
export const createActionDeleteVisueleInspectieAfbeeldingByAfspraakId = (afspraakId: number): DeleteVisueleInspectieAfbeeldingByAfspraakId => ({
    type: DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID,
    afspraakId: afspraakId,
});

export const VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID = 'VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID';
export type VulVisueleInspectieAfbeeldingByAfspraakIdAction = { type: 'VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID', afspraakId: number, visueleInspectieAfbeelding: AnnotatieAfbeelding };
export const createActionVulVisueleInspectieAfbeeldingByAfspraakId = (
    afspraakId: number, visueleInspectieAfbeelding: AnnotatieAfbeelding): VulVisueleInspectieAfbeeldingByAfspraakIdAction => ({
    type: VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID,
    afspraakId: afspraakId,
    visueleInspectieAfbeelding: visueleInspectieAfbeelding,
});

export const MAAK_VISUELE_INSPECTIE_ICOON = 'MAAK_VISUELE_INSPECTIE_ICOON';
export type MaakVisueleInsopectieIcoonAction = { type: 'MAAK_VISUELE_INSPECTIE_ICOON', afspraakId: number, icoonId: number, icoonType: AnnotatieIcoonType, x: number, y: number }
export const createActionMaakVisueleInspectieIcoon = (
    afspraakId: number, icoonType: AnnotatieIcoonType, x: number, y: number): MaakVisueleInsopectieIcoonAction => ({
    type: MAAK_VISUELE_INSPECTIE_ICOON,
    afspraakId: afspraakId,
    icoonId: getNextIcoonId(),
    icoonType: icoonType,
    x: x,
    y: y,
});

export const SET_VISUELE_INSPECTIE_ICOON_POSITION = 'SET_VISUELE_INSPECTIE_ICOON_POSITION';
export type SetVisueleInspectieIcoonPositionAction = { type: 'SET_VISUELE_INSPECTIE_ICOON_POSITION', afspraakId: number, icoonId: number, x: number, y: number };
export const createActionSetVisueleInspectieIcoonPosition = (afspraakId: number, icoonId: number, x: number, y: number): SetVisueleInspectieIcoonPositionAction => ({
    type: SET_VISUELE_INSPECTIE_ICOON_POSITION,
    afspraakId: afspraakId,
    icoonId: icoonId,
    x: x,
    y: y,
});

export const SET_VISUELE_INSPECTIE_ICOON_TEKST = 'SET_VISUELE_INSPECTIE_ICOON_TEKST';
export type SetVisueleInspectieIcoonTekstAction = { type: 'SET_VISUELE_INSPECTIE_ICOON_TEKST', afspraakId: number, icoonId: number, tekst: string };
export const createActionSetVisueleInspectieIcoonTekst = (afspraakId: number, icoonId: number, tekst: string): SetVisueleInspectieIcoonTekstAction => ({
    type: SET_VISUELE_INSPECTIE_ICOON_TEKST,
    afspraakId: afspraakId,
    icoonId: icoonId,
    tekst: tekst,
});

export const VERWIJDER_VISUELE_INSPECTIE_ICOON = 'VERWIJDER_VISUELE_INSPECTIE_ICOON';
export type VerwijderVisueleInspectieIcoonAction = { type: 'VERWIJDER_VISUELE_INSPECTIE_ICOON', afspraakId: number, icoonId: number };
export const createActionVerwijderVisueleInspectieIcoon = (afspraakId: number, icoonId: number): VerwijderVisueleInspectieIcoonAction => ({
    type: VERWIJDER_VISUELE_INSPECTIE_ICOON,
    afspraakId: afspraakId,
    icoonId: icoonId,
});
