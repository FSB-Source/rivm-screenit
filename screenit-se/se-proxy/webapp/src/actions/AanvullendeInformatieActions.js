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

import type {OnvolledigOnderzoekOption} from '../datatypes/visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek';
import type {OnderbrokenOnderzoekOption} from '../datatypes/visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek';
import type {ExtraFotosReden} from '../datatypes/visueleinspectie/aanvullendeinformatie/ExtraFotosReden';
import type {Amputatie} from '../datatypes/Onderzoek';

export type AanvullendeInformatieActions =
    EerderMammogramJaartalAction
    | EerderMammogramZorginstellingAction
    | OnvolledigOnderzoekAction
    | OnderbrokenOnderzoekAction
    | ExtraFotosRedenenAction
    | DubbeleTijdAction
    | DubbeleTijdRedenAction
    | AdviesHuisartsAction
    | SetAmputatieAction;

export const MAAK_EERDER_MAMMOGRAM_JAARTAL = 'MAAK_EERDER_MAMMOGRAM_JAARTAL';
export type EerderMammogramJaartalAction = { type: 'MAAK_EERDER_MAMMOGRAM_JAARTAL', afspraakId: number, eerderMammogramJaartal: ?number };
export const createActionMaakEerderMammogramJaartal = (afspraakId: number, jaartal: ?number): EerderMammogramJaartalAction => ({
    type: MAAK_EERDER_MAMMOGRAM_JAARTAL,
    afspraakId: afspraakId,
    eerderMammogramJaartal: jaartal,
});

export const MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING = 'MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING';
export type EerderMammogramZorginstellingAction = { type: 'MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING', afspraakId: number, eerderMammogramZorginstellingId: ?number };
export const createActionMaakEerderMammogramZorginstelling = (afspraakId: number, eerderMammogramZorginstellingId: ?number): EerderMammogramZorginstellingAction => ({
    type: MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING,
    afspraakId: afspraakId,
    eerderMammogramZorginstellingId: eerderMammogramZorginstellingId,
});

export const MAAK_ONVOLLEDIG_ONDERZOEK = 'MAAK_ONVOLLEDIG_ONDERZOEK';
export type OnvolledigOnderzoekAction = { type: 'MAAK_ONVOLLEDIG_ONDERZOEK', afspraakId: number, onvolledigOnderzoek: ?OnvolledigOnderzoekOption };
export const createActionMaakOnvolledigOnderzoek = (afspraakId: number, onvolledigOnderzoek: ?OnvolledigOnderzoekOption): OnvolledigOnderzoekAction => ({
    type: MAAK_ONVOLLEDIG_ONDERZOEK,
    afspraakId: afspraakId,
    onvolledigOnderzoek: onvolledigOnderzoek,
});

export const MAAK_ONDERBROKEN_ONDERZOEK = 'MAAK_ONDERBROKEN_ONDERZOEK';
export type OnderbrokenOnderzoekAction = { type: 'MAAK_ONDERBROKEN_ONDERZOEK', afspraakId: number, onderbrokenOnderzoek: ?OnderbrokenOnderzoekOption };
export const createActionMaakOnderbrokenOnderzoek = (afspraakId: number, onderbrokenOnderzoek: ?OnderbrokenOnderzoekOption): OnderbrokenOnderzoekAction => ({
    type: MAAK_ONDERBROKEN_ONDERZOEK,
    afspraakId: afspraakId,
    onderbrokenOnderzoek: onderbrokenOnderzoek,
});

export const MAAK_EXTRA_FOTOS_REDENEN = 'MAAK_EXTRA_FOTOS_REDENEN';
export type ExtraFotosRedenenAction = { type: 'MAAK_EXTRA_FOTOS_REDENEN', afspraakId: number, extraFotosRedenen: ?Array<ExtraFotosReden> };
export const createActionMaakExtraFotosRedenen = (afspraakId: number, extraFotosRedenen: ?Array<ExtraFotosReden>): ExtraFotosRedenenAction => ({
    type: MAAK_EXTRA_FOTOS_REDENEN,
    afspraakId: afspraakId,
    extraFotosRedenen: extraFotosRedenen,
});

export const MAAK_DUBBELE_TIJD = 'MAAK_DUBBELE_TIJD';
export type DubbeleTijdAction = { type: 'MAAK_DUBBELE_TIJD', afspraakId: number, clientId: number, dubbeleTijd: boolean };
export const createActionMaakDubbeleTijd = (afspraakId: number, clientId: number, dubbeleTijd: boolean): DubbeleTijdAction => ({
    type: MAAK_DUBBELE_TIJD,
    afspraakId: afspraakId,
    clientId: clientId,
    dubbeleTijd: dubbeleTijd,
});

export const MAAK_DUBBELE_TIJD_REDEN = 'MAAK_DUBBELE_TIJD_REDEN';
export type DubbeleTijdRedenAction = { type: 'MAAK_DUBBELE_TIJD_REDEN', afspraakId: number, clientId: number, dubbeleTijdReden: ?string };
export const createActionMaakDubbeleTijdReden = (afspraakId: number, clientId: number, dubbeleTijdReden: ?string): DubbeleTijdRedenAction => ({
    type: MAAK_DUBBELE_TIJD_REDEN,
    afspraakId: afspraakId,
    clientId: clientId,
    dubbeleTijdReden: dubbeleTijdReden,
});

export const MAAK_ADVIES_HUISARTS = 'MAAK_ADVIES_HUISARTS';
export type AdviesHuisartsAction = { type: 'MAAK_ADVIES_HUISARTS', afspraakId: number, adviesHuisarts: string };
export const createActionMaakAdviesHuisarts = (afspraakId: number, adviesHuisarts: string): AdviesHuisartsAction => ({
    type: MAAK_ADVIES_HUISARTS,
    afspraakId: afspraakId,
    adviesHuisarts: adviesHuisarts,
});

export const SET_AMPUTATIE = 'SET_AMPUTATIE';
export type SetAmputatieAction = { type: 'SET_AMPUTATIE', afspraakId: number, amputatie: ?Amputatie };
export const createActionSetAmputatie = (afspraakId: number, amputatie: ?Amputatie): SetAmputatieAction => ({
    type: SET_AMPUTATIE,
    afspraakId: afspraakId,
    amputatie: amputatie,
});
