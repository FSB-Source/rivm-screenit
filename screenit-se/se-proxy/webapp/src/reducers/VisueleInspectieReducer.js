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

import {getIfExists, getMandatory} from '../util/MapUtil';
import type {AnnotatieAfbeelding} from '../datatypes/AnnotatieAfbeelding';
import {getNextIcoonId, mapAfbeeldingDtoToAfbeelding} from '../datatypes/AnnotatieAfbeelding';
import type {AnnotatieIcoon, AnnotatieIcoonDto} from '../datatypes/AnnotatieIcoon';
import type {VisueleInspectieActions} from '../actions/VisueleInspectieActions';
import {
    DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID,
    MAAK_VISUELE_INSPECTIE_ICOON,
    MAMMOGRAFIE_OPSLAAN,
    MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG,
    SET_VISUELE_INSPECTIE_AFBEELDING,
    SET_VISUELE_INSPECTIE_ICOON_POSITION,
    SET_VISUELE_INSPECTIE_ICOON_TEKST,
    VERWIJDER_VISUELE_INSPECTIE_ICOON,
    VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID,
    VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID,
} from '../actions/VisueleInspectieActions';
import {CLEAR_CACHE} from '../actions/ClearCacheActions';

const visueleInspectieIcoonReducer = (stateSlice: Map<number, AnnotatieAfbeelding> = new Map(), action: VisueleInspectieActions): Map<number, AnnotatieAfbeelding> => {
    const result: Map<number, AnnotatieAfbeelding> = new Map();

    const iconenById: Map<number, AnnotatieIcoon> = new Map();
    switch (action.type) {
        case VUL_VISUELE_INSPECTIE_AFBEELDINGEN_BY_AFSPRAAK_ID:
            action.visueleInspectieAfbeeldingen.forEach(
                (visueleInspectieAfbeelding: AnnotatieAfbeelding) => {
                    result.set(visueleInspectieAfbeelding.afspraakId, visueleInspectieAfbeelding);
                },
            );
            return new Map([...stateSlice, ...result]);
        case VUL_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID:
            result.set(action.afspraakId, action.visueleInspectieAfbeelding);
            break;
        case SET_VISUELE_INSPECTIE_AFBEELDING:
            result.set(action.afspraakId, mapAfbeeldingDtoToAfbeelding(action.afspraakId, action.visueleInspectieAfbeeldingDto));
            break;
        case MAAK_VISUELE_INSPECTIE_ICOON:
            iconenById.set(action.icoonId,
                {
                    icoonId: action.icoonId,
                    afspraakId: action.afspraakId,
                    type: action.icoonType,
                    positieX: action.x,
                    positieY: action.y,
                    tekst: '',
                    nieuwIcoon: true,
                });
            kopieerAfbeelding(action.afspraakId);
            break;
        case SET_VISUELE_INSPECTIE_ICOON_POSITION:
            iconenById.set(action.icoonId, {
                ...getMandatory(getMandatory(stateSlice, action.afspraakId).iconenById, action.icoonId),
                ...{positieX: action.x, positieY: action.y},
            });
            kopieerAfbeelding(action.afspraakId);
            break;
        case SET_VISUELE_INSPECTIE_ICOON_TEKST:
            iconenById.set(action.icoonId, {
                ...getMandatory(getMandatory(stateSlice, action.afspraakId).iconenById, action.icoonId),
                ...{tekst: action.tekst},
            });
            kopieerAfbeelding(action.afspraakId);
            break;
        case VERWIJDER_VISUELE_INSPECTIE_ICOON:
            kopieerAfbeelding(action.afspraakId);
            const afbeelding: AnnotatieAfbeelding = getMandatory(result, action.afspraakId);
            afbeelding.iconenById.delete(action.icoonId);
            break;
        case MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG: 
        case MAMMOGRAFIE_OPSLAAN:
            action.mammografie.visueleInspectieAfbeelding.iconen.forEach((icoon: AnnotatieIcoonDto) => {
                const icoonId: number = getNextIcoonId();
                iconenById.set(icoonId, {...icoon, ...{icoonId}});
            });
            break;
        case DELETE_VISUELE_INSPECTIE_AFBEELDING_BY_AFSPRAAK_ID:
            stateSlice.delete(action.afspraakId);
            return stateSlice;
        case CLEAR_CACHE:
            return new Map();
        default:
            return stateSlice;

    }

    return new Map([...stateSlice, ...result]);

    function kopieerAfbeelding(afspraakId: number) {
        let bestaandeAfbeelding: ?AnnotatieAfbeelding = getIfExists(stateSlice, afspraakId);
        if (bestaandeAfbeelding === null || bestaandeAfbeelding === undefined || bestaandeAfbeelding.iconenById === null) {
            result.set(afspraakId,
                {afspraakId: afspraakId, iconenById: new Map([...iconenById])},
            );
        } else {
            result.set(afspraakId,
                {afspraakId: afspraakId, iconenById: new Map([...bestaandeAfbeelding.iconenById, ...iconenById])},
            );
        }
    }
};

export default visueleInspectieIcoonReducer;
