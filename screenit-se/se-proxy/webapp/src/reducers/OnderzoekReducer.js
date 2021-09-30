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

import type {Onderzoek, OnderzoekDto} from '../datatypes/Onderzoek';
import type {OnderzoekActions} from '../actions/OnderzoekActions';
import {ONDERZOEK_AFRONDEN, ONDERZOEK_OPSLAAN, ONDERZOEK_STARTEN, VUL_ONDERZOEK_BY_AFSPRAAK_ID} from '../actions/OnderzoekActions';
import {getIfExists, getMandatory} from '../util/MapUtil';
import type {AfspraakDto} from '../datatypes/Afspraak';
import type {MBBSignaleringActions} from '../actions/MBBSignaleringActions';
import {
    MAAK_AANVULLENDE_INFORMATIE_OPERATIE,
    MAAK_EXTRA_MEDEWERKER,
    MAAK_MBB_OPMERKING,
    MAAK_RADIOLOOG_OPMERKING,
    MAAK_REDEN_FOTOBESPREKING,
    MAAK_SUBOPTIMALE_INSTELTECHNIEK,
    OPERATIE_LINKS,
    OPERATIE_RECHTS,
} from '../actions/MBBSignaleringActions';
import type {AanvullendeInformatieActions} from '../actions/AanvullendeInformatieActions';
import {
    MAAK_ADVIES_HUISARTS,
    MAAK_EERDER_MAMMOGRAM_JAARTAL,
    MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING,
    MAAK_EXTRA_FOTOS_REDENEN,
    MAAK_ONDERBROKEN_ONDERZOEK,
    MAAK_ONVOLLEDIG_ONDERZOEK,
    SET_AMPUTATIE,
} from '../actions/AanvullendeInformatieActions';
import type {ClearCacheActions} from '../actions/ClearCacheActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const onderzoekFromDto = (onderzoekDto: OnderzoekDto) => (onderzoekDto: Onderzoek);

const onderzoekReducer = (
    stateSlice: Map<number, Onderzoek> = new Map(),
    action: AanvullendeInformatieActions | ClearCacheActions | MBBSignaleringActions | OnderzoekActions): Map<number, Onderzoek> => {
    const result: Map<number, Onderzoek> = new Map();

    switch (action.type) {
        case VUL_ONDERZOEK_BY_AFSPRAAK_ID:
            action.afspraken.forEach(
                (afspraak: AfspraakDto) => {
                    if (afspraak.huidigOnderzoek)
                        result.set(afspraak.id, onderzoekFromDto(afspraak.huidigOnderzoek));
                },
            );
            break;
        case ONDERZOEK_STARTEN:
            result.set(action.afspraakId, {
                eerderMammogramZorginstellingId: null,
                eerderMammogramJaartal: null,
                suboptimaleInsteltechniek: null,
                redenFotobespreking: null,
                extraMedewerkerId: null,
                opmerkingMbber: null,
                opmerkingVoorRadioloog: null,
                operatieRechts: false,
                operatieLinks: false,
                amputatie: action.amputatie,
                aanvullendeInformatieOperatie: null,
                status: 'ACTIEF',
                onvolledigOnderzoek: null,
                onderbrokenOnderzoek: null,
                extraFotosRedenen: null,
                adviesHuisarts: null,
            });
            break;
        case ONDERZOEK_OPSLAAN:
            result.set(action.afspraakId, action.onderzoek);
            break;
        case ONDERZOEK_AFRONDEN:
            result.set(action.afspraakId, {...getMandatory(stateSlice, action.afspraakId), status: action.status});
            break;
        case MAAK_EERDER_MAMMOGRAM_ZORGINSTELLING:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{eerderMammogramZorginstellingId: action.eerderMammogramZorginstellingId}});
            break;
        case MAAK_EERDER_MAMMOGRAM_JAARTAL:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{eerderMammogramJaartal: action.eerderMammogramJaartal}});
            break;
        case MAAK_SUBOPTIMALE_INSTELTECHNIEK:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{suboptimaleInsteltechniek: action.suboptimaleInsteltechniek}});
            break;
        case MAAK_REDEN_FOTOBESPREKING:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{redenFotobespreking: action.redenFotobespreking}});
            break;
        case MAAK_EXTRA_MEDEWERKER:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{extraMedewerkerId: action.extraMedewerkerId}});
            break;
        case MAAK_MBB_OPMERKING:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{opmerkingMbber: action.opmerkingMbber}});
            break;
        case MAAK_RADIOLOOG_OPMERKING:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{opmerkingVoorRadioloog: action.opmerkingVoorRadioloog}});
            break;
        case OPERATIE_RECHTS:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{operatieRechts: action.operatieRechts}});
            break;
        case OPERATIE_LINKS:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{operatieLinks: action.operatieLinks}});
            break;
        case SET_AMPUTATIE:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{amputatie: action.amputatie}});
            break;
        case MAAK_AANVULLENDE_INFORMATIE_OPERATIE:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{aanvullendeInformatieOperatie: action.aanvullendeInformatieOperatie}});
            break;
        case MAAK_ONVOLLEDIG_ONDERZOEK:
            result.set(action.afspraakId,
                {...getIfExists(stateSlice, action.afspraakId), ...{onvolledigOnderzoek: action.onvolledigOnderzoek, onderbrokenOnderzoek: null, status: 'ONVOLLEDIG'}});
            break;
        case MAAK_ONDERBROKEN_ONDERZOEK:
            result.set(action.afspraakId,
                {...getIfExists(stateSlice, action.afspraakId), ...{onvolledigOnderzoek: null, onderbrokenOnderzoek: action.onderbrokenOnderzoek, status: 'ONDERBROKEN'}});
            break;
        case MAAK_EXTRA_FOTOS_REDENEN:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{extraFotosRedenen: action.extraFotosRedenen}});
            break;
        case MAAK_ADVIES_HUISARTS:
            result.set(action.afspraakId, {...getIfExists(stateSlice, action.afspraakId), ...{adviesHuisarts: action.adviesHuisarts}});
            break;
        case CLEAR_CACHE:
            return new Map();
        default:
            return stateSlice;
    }

    return new Map([...stateSlice, ...result]);
};

export default onderzoekReducer;
