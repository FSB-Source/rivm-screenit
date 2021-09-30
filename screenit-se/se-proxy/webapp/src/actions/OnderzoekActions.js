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

import type {Amputatie, Onderzoek, Onderzoekstatus} from '../datatypes/Onderzoek';
import type {AfspraakDto} from '../datatypes/Afspraak';

export type OnderzoekActions = VulOnderzoekByAfspraakIdAction | OnderzoekStartenAction | OnderzoekOpslaanAction | OnderzoekAfrondenAction;

export const VUL_ONDERZOEK_BY_AFSPRAAK_ID = 'VUL_ONDERZOEK_BY_AFSPRAAK_ID';
export type VulOnderzoekByAfspraakIdAction = { type: 'VUL_ONDERZOEK_BY_AFSPRAAK_ID', afspraken: Array<AfspraakDto> };
export const createActionVulOnderzoekByAfspraakId = (afspraken: Array<AfspraakDto>): VulOnderzoekByAfspraakIdAction => ({
    type: VUL_ONDERZOEK_BY_AFSPRAAK_ID,
    afspraken: afspraken,
});

export const ONDERZOEK_STARTEN = 'ONDERZOEK_STARTEN';
export type OnderzoekStartenAction = { type: 'ONDERZOEK_STARTEN', afspraakId: number, amputatie: ?Amputatie };
export const createActionOnderzoekStarten = (afspraakId: number, amputatie: ?Amputatie): OnderzoekStartenAction => {
    return {
        type: ONDERZOEK_STARTEN,
        afspraakId: afspraakId,
        amputatie: amputatie,
    };
};

export const ONDERZOEK_OPSLAAN = 'ONDERZOEK_OPSLAAN';
export type OnderzoekOpslaanAction = { type: 'ONDERZOEK_OPSLAAN', afspraakId: number, onderzoek: Onderzoek };
export const createActionOnderzoekOpslaan = (afspraakId: number, onderzoek: Onderzoek): OnderzoekOpslaanAction => {
    return {
        type: ONDERZOEK_OPSLAAN,
        afspraakId: afspraakId,
        onderzoek: onderzoek,
    };
};

export const ONDERZOEK_AFRONDEN = 'ONDERZOEK_AFRONDEN';
export type OnderzoekAfrondenAction = { type: 'ONDERZOEK_AFRONDEN', afspraakId: number, status: Onderzoekstatus };
export const createActionOnderzoekAfronden = (afspraakId: number, status: Onderzoekstatus): OnderzoekAfrondenAction => {
    return {
        type: ONDERZOEK_AFRONDEN,
        afspraakId: afspraakId,
        status: status,
    };
};
