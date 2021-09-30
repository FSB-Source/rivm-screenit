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

import type {Afspraak, Identificatiesoort} from '../datatypes/Afspraak';
import type {Client} from '../datatypes/Client';
import type {Signalering, SignaleringDto} from '../datatypes/Signalering';
import {mapSignaleringToDto} from '../datatypes/Signalering';
import type {GeenHuisartsOption} from '../datatypes/Huisarts';
import type {TijdelijkAdres} from '../datatypes/TijdelijkAdres';

export type AfspraakActions = VulAfsprakenAction |
    ClearAfsprakenAction |
    BaseClientGegevensAction |
    UitschrijvenAction |
    KiesIdentificatiesoortAction |
    KiesIdentificatienummerAction |
    AfspraakSignalerenAction |
    AfspraakAfrondenAction |
    AfspraakSignalerenOpslaanAction |
    AfspraakDoorvoerenAction |
    BezwaarAanvragenAction;

type BaseClientGegevensAction = {
    type: string,
    afspraakId: number,
    clientId: number,
    identificatiesoort: ?Identificatiesoort,
    identificatienummer: ?string,
    bezwaarAangevraagd: boolean,
    tijdelijkAdres: ?TijdelijkAdres,
    emailadres: ?string,
    telefoonnummer1: ?string,
    telefoonnummer2: ?string,
    huisartsId: ?number,
    geenHuisartsOptie: ?GeenHuisartsOption
}
const baseClientGegevensAction = (afspraak: Afspraak, client: Client, actieType: string): BaseClientGegevensAction => {
    return {
        type: actieType,
        afspraakId: afspraak.id,
        clientId: client.id,
        identificatiesoort: afspraak.identificatiesoort,
        identificatienummer: afspraak.identificatienummer,
        bezwaarAangevraagd: afspraak.bezwaarAangevraagd,
        tijdelijkAdres: client.tijdelijkAdres,
        emailadres: client.emailadres,
        telefoonnummer1: client.telefoonnummer1,
        telefoonnummer2: client.telefoonnummer2,
        huisartsId: afspraak.huisartsId,
        geenHuisartsOptie: afspraak.geenHuisartsOptie,
    };
};

export const INSCHRIJVEN = 'INSCHRIJVEN';
export const createActionInschrijven = (afspraak: Afspraak, client: Client): BaseClientGegevensAction => {
    return baseClientGegevensAction(afspraak, client, INSCHRIJVEN);
};

export const CLIENTGEGEVENS_OPSLAAN = 'CLIENTGEGEVENS_OPSLAAN';
export const createActionClientgegevensOpslaan = (afspraak: Afspraak, client: Client): BaseClientGegevensAction => {
    return baseClientGegevensAction(afspraak, client, CLIENTGEGEVENS_OPSLAAN);
};

export const UITSCHRIJVEN = 'UITSCHRIJVEN';
export type UitschrijvenAction = { type: 'UITSCHRIJVEN', afspraakId: number }
export const createActionUitschrijven = (afspraakId: number): UitschrijvenAction => ({
    type: UITSCHRIJVEN,
    afspraakId: afspraakId,
});

export const KIES_IDENTIFICATIESOORT = 'KIES_IDENTIFICATIESOORT';
export type KiesIdentificatiesoortAction = { type: 'KIES_IDENTIFICATIESOORT', afspraakId: number, identificatiesoort: Identificatiesoort }
export const createActionKiesIdentificatiesoort = (afspraakId: number, identificatiesoort: Identificatiesoort): KiesIdentificatiesoortAction => {
    return {
        type: KIES_IDENTIFICATIESOORT,
        afspraakId: afspraakId,
        identificatiesoort: identificatiesoort,
    };
};

export const KIES_IDENTIFICATIENUMMER = 'KIES_IDENTIFICATIENUMMER';
export type KiesIdentificatienummerAction = { type: 'KIES_IDENTIFICATIENUMMER', afspraakId: number, identificatienummer: ?string }
export const createActionKiesIdentificatienummer = (afspraakId: number, identificatienummer: ?string): KiesIdentificatienummerAction => {
    return {
        type: KIES_IDENTIFICATIENUMMER,
        afspraakId: afspraakId,
        identificatienummer: identificatienummer,
    };
};

export const AFSPRAAK_SIGNALEREN = 'AFSPRAAK_SIGNALEREN';
export type AfspraakSignalerenAction = { type: 'AFSPRAAK_SIGNALEREN', afspraakId: number }
export const createActionAfspraakSignaleren = (afspraakId: number): AfspraakSignalerenAction => {
    return {
        type: AFSPRAAK_SIGNALEREN,
        afspraakId: afspraakId,
    };
};

export const AFSPRAAK_AFRONDEN = 'AFSPRAAK_AFRONDEN';
export type AfspraakAfrondenAction = { type: 'AFSPRAAK_AFRONDEN', afspraakId: number, signaleren: SignaleringDto }
export const createActionAfspraakAfronden = (afspraakId: number, signalering: Signalering): AfspraakAfrondenAction => {
    return {
        type: AFSPRAAK_AFRONDEN,
        afspraakId: afspraakId,
        signaleren: mapSignaleringToDto(signalering),
    };
};

export const AFSPRAAK_SIGNALEREN_OPSLAAN = 'AFSPRAAK_SIGNALEREN_OPSLAAN';
export type AfspraakSignalerenOpslaanAction = { type: 'AFSPRAAK_SIGNALEREN_OPSLAAN', afspraakId: number, signaleren: SignaleringDto }
export const createActionAfspraakSignalerenOpslaan = (afspraakId: number, signalering: Signalering): AfspraakSignalerenOpslaanAction => {
    return {
        type: AFSPRAAK_SIGNALEREN_OPSLAAN,
        afspraakId: afspraakId,
        signaleren: mapSignaleringToDto(signalering),
    };
};

export const AFSPRAAK_DOORVOEREN = 'AFSPRAAK_DOORVOEREN';
export type AfspraakDoorvoerenAction = { type: 'AFSPRAAK_DOORVOEREN', afspraakId: number }
export const createActionAfspraakDoorvoeren = (afspraakId: number): AfspraakDoorvoerenAction => {
    return {
        type: AFSPRAAK_DOORVOEREN,
        afspraakId: afspraakId,
    };
};

export const VUL_AFSPRAKEN = 'VUL_AFSPRAKEN';
export type VulAfsprakenAction = { type: 'VUL_AFSPRAKEN', afspraken: Array<Afspraak> }
export const createActionVulAfspraken = (afspraken: Array<Afspraak>): VulAfsprakenAction => ({
    type: VUL_AFSPRAKEN,
    afspraken: afspraken,
});

export const CLEAR_AFSPRAKEN = 'CLEAR_AFSPRAKEN';
export type ClearAfsprakenAction = { type: 'CLEAR_AFSPRAKEN', datum: string};
export const createActionClearAfspraken = (datum: string): ClearAfsprakenAction => {
    return {
        type: CLEAR_AFSPRAKEN,
        datum: datum
    }
};

export const BEZWAAR_AANVRAGEN = 'BEZWAAR_AANVRAGEN';
export type BezwaarAanvragenAction = { type: 'BEZWAAR_AANVRAGEN', afspraakId: number, bezwaarAangevraagd: boolean }
export const createActionBezwaarAanvragen = (afspraakId: number, bezwaarAangevraagd: boolean): BezwaarAanvragenAction => ({
    type: BEZWAAR_AANVRAGEN,
    afspraakId: afspraakId,
    bezwaarAangevraagd: bezwaarAangevraagd,
});
