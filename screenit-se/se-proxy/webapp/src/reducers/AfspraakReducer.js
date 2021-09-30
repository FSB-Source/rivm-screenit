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

import type {AfspraakActions} from '../actions/AfspraakActions';
import {
    AFSPRAAK_AFRONDEN,
    AFSPRAAK_DOORVOEREN,
    AFSPRAAK_SIGNALEREN,
    BEZWAAR_AANVRAGEN,
    CLIENTGEGEVENS_OPSLAAN,
    INSCHRIJVEN,
    KIES_IDENTIFICATIENUMMER,
    KIES_IDENTIFICATIESOORT,
    UITSCHRIJVEN,
    VUL_AFSPRAKEN,
    CLEAR_AFSPRAKEN
} from '../actions/AfspraakActions';
import {Afspraak} from '../datatypes/Afspraak';
import {KIES_GEEN_HUISARTS_OPTIE, KIES_HUISARTS} from '../actions/HuisartsActions';
import type {ClearAfsprakenAction} from '../actions/AfspraakActions';
import {ONDERZOEK_STARTEN} from '../actions/OnderzoekActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

type AfspraakMap = Map<number, Afspraak>;

const afsprakenReducer = (stateSlice: AfspraakMap = new Map(), action: AfspraakActions): AfspraakMap => {
    let result: Map<number, Afspraak> = new Map();
    if (action.type === VUL_AFSPRAKEN) {
        if (action.afspraken) {
            const afspraken = ((action.afspraken: any): Array<Afspraak>);
            for (const afspraak: Afspraak of afspraken) {
                result.set(afspraak.id, afspraak);
            }
        }
    } else if (action.type === CLEAR_AFSPRAKEN) {
        const clearAction : ClearAfsprakenAction = ((action: any): ClearAfsprakenAction);
        for (const afspraak: Afspraak of stateSlice.values()) {
            if (afspraak.vanafDatum !== clearAction.datum) {
                result.set(afspraak.id, afspraak);
            }
        }
        return result;
    } else {
        const afspraak: Afspraak | void = stateSlice.get(action.afspraakId);
        if (afspraak) {
            const copyAfspraak: Afspraak = Object.create(afspraak);
            switch (action.type) {
                case INSCHRIJVEN:
                    copyAfspraak.status = 'INGESCHREVEN';
                case CLIENTGEGEVENS_OPSLAAN:
                    copyAfspraak.bezwaarAangevraagd = action.bezwaarAangevraagd;
                    copyAfspraak.geenHuisartsOptie = action.geenHuisartsOptie;
                    copyAfspraak.huisartsId = action.huisartsId;
                    copyAfspraak.identificatienummer = action.identificatienummer;
                    copyAfspraak.identificatiesoort = action.identificatiesoort;
                    break;
                case UITSCHRIJVEN:
                    copyAfspraak.status = 'VERWACHT';
                    break;
                case ONDERZOEK_STARTEN:
                    copyAfspraak.status = 'ONDERZOEK';
                    break;
                case AFSPRAAK_SIGNALEREN:
                    copyAfspraak.status = 'SIGNALEREN';
                    break;
                case AFSPRAAK_AFRONDEN:
                    copyAfspraak.status = 'BEEINDIGD';
                    break;
                case KIES_IDENTIFICATIESOORT:
                    copyAfspraak.identificatiesoort = action.identificatiesoort;
                    break;
                case KIES_IDENTIFICATIENUMMER:
                    copyAfspraak.identificatienummer = action.identificatienummer;
                    break;
                case BEZWAAR_AANVRAGEN:
                    copyAfspraak.bezwaarAangevraagd = action.bezwaarAangevraagd;
                    break;
                case KIES_HUISARTS:
                    copyAfspraak.huisartsId = action.huisartsId;
                    copyAfspraak.geenHuisartsOptie = null;
                    break;
                case KIES_GEEN_HUISARTS_OPTIE:
                    copyAfspraak.geenHuisartsOptie = action.geenHuisartsOptie;
                    copyAfspraak.huisartsId = null;
                    break;
                case AFSPRAAK_DOORVOEREN:
                    copyAfspraak.doorgevoerd = true;
                    break;
                case CLEAR_CACHE:
                    return new Map();
                default:
                    break;
            }
            result.set(action.afspraakId, copyAfspraak);
        }
    }
    return new Map([...stateSlice, ...result]);
};

export default afsprakenReducer;
