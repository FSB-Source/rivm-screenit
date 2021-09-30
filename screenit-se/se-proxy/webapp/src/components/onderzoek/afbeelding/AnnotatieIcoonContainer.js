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

import {connect} from 'react-redux';
import type {AnnotatieIcoonProps} from './AnnotatieIcoonView';
import AnnotatieIcoonView from './AnnotatieIcoonView';
import type {State} from '../../../datatypes/State';
import {
    createActionMaakVisueleInspectieIcoon,
    createActionSetVisueleInspectieIcoonPosition,
    createActionSetVisueleInspectieIcoonTekst,
    createActionVerwijderVisueleInspectieIcoon,
} from '../../../actions/VisueleInspectieActions';
import {getAfbeeldingByType} from '../../../util/IcoonAfbeeldingTypeUtil';
import type {AnnotatieIcoonType} from '../../../datatypes/AnnotatieIcoon';
import {
    createActionMaakSignaleringIcoonLinksHorizontaal,
    createActionMaakSignaleringIcoonLinksVerticaal,
    createActionMaakSignaleringIcoonRechtsHorizontaal,
    createActionMaakSignaleringIcoonRechtsVerticaal,
    createActionSetSignaleringIcoonPositionLinksHorizontaal,
    createActionSetSignaleringIcoonPositionLinksVerticaal,
    createActionSetSignaleringIcoonPositionRechtsHorizontaal,
    createActionSetSignaleringIcoonPositionRechtsVerticaal,
    createActionVerwijderSignaleringIcoonLinksHorizontaal,
    createActionVerwijderSignaleringIcoonLinksVerticaal,
    createActionVerwijderSignaleringIcoonRechtsHorizontaal,
    createActionVerwijderSignaleringIcoonRechtsVerticaal,
} from '../../../actions/SignalerenActions';
import {dispatchActions} from '../../../util/DispatchUtil';
import {createActionSetAmputatie} from '../../../actions/AanvullendeInformatieActions';
import {store} from '../../../Store';
import type {Amputatie} from '../../../datatypes/Onderzoek';
import {showErrorToast} from '../../../util/ToastUtil';
import type {AnnotatieAfbeelding} from '../../../datatypes/AnnotatieAfbeelding';
import {verbodenWegensAmputatie, verbodenWegensSignaleringsicoon} from '../OnderzoekContainer';

const isVisueleInspectie = (icoonType: AnnotatieIcoonType): boolean => {
    return !(icoonType === 'SIGNALERING_MASSA' || icoonType === 'SIGNALERING_ARCHITECTUURVERSTORING' || icoonType === 'SIGNALERING_ASYMMETRIE' ||
        icoonType === 'SIGNALERING_CALCIFICATIES');
};

export type Aanzicht = 'RECHTS_VERTICAAL' | 'LINKS_VERTICAAL' | 'RECHTS_HORIZONTAAL' | 'LINKS_HORIZONTAAL';

const amputatieScaleFactor = 3;

const mapStateToProps = (state: State, ownProps: AnnotatieIcoonProps) => {
    const icoonWidth: number = getAfbeeldingByType(ownProps.icoon.type).width;
    const icoonHeight: number = getAfbeeldingByType(ownProps.icoon.type).height;
    const isNietVisueleInspectie: boolean = state.navigation.subPagina !== 'Visuele inspectie';

    return {
        icoon: ownProps.icoon,
        icoonWidth: icoonWidth,
        icoonHeight: icoonHeight,
        amputatieSize: ownProps.imageWidth / amputatieScaleFactor,
        isDraggable: ownProps.isDraggable,
        isNietVisueleInspectie: isNietVisueleInspectie,
    };
};

const mapDispatchToProps = (dispatch, parentProps: AnnotatieIcoonProps) => {
    return {
        verwijderIcoon(afspraakId: number, icoonId: number, x: number, y: number, aanzicht: Aanzicht) {
            if (isVisueleInspectie(parentProps.icoon.type)) {
                if (parentProps.icoon.type === 'AMPUTATIE') {
                    dispatchActions(dispatch, createActionSetAmputatie(afspraakId, null));
                } else {
                    dispatchActions(dispatch, createActionVerwijderVisueleInspectieIcoon(afspraakId, icoonId));
                }
            } else {
                switch (aanzicht) {
                    case 'RECHTS_VERTICAAL':
                        dispatchActions(dispatch, createActionVerwijderSignaleringIcoonRechtsVerticaal(afspraakId, icoonId));
                        break;
                    case 'LINKS_VERTICAAL':
                        dispatchActions(dispatch, createActionVerwijderSignaleringIcoonLinksVerticaal(afspraakId, icoonId));
                        break;
                    case 'RECHTS_HORIZONTAAL':
                        dispatchActions(dispatch, createActionVerwijderSignaleringIcoonRechtsHorizontaal(afspraakId, icoonId));
                        break;
                    case 'LINKS_HORIZONTAAL':
                        dispatchActions(dispatch, createActionVerwijderSignaleringIcoonLinksHorizontaal(afspraakId, icoonId));
                        break;
                    default:
                        console.error('onbekend aanzicht: ' + aanzicht);
                }
            }
        },
        setPosition(afspraakId: number, icoonId: number, x: number, y: number, aanzicht: Aanzicht) {
            if (isVisueleInspectie(parentProps.icoon.type)) {
                const amputatie = x2amputatie(x);
                if (parentProps.icoon.type === 'AMPUTATIE') {
                    if (!verbodenWegensSignaleringsicoon(afspraakId, amputatie)) {
                        setAnnotatieIndienMogelijk(afspraakId, amputatie, dispatch);
                    }
                } else {
                    if (!verbodenWegensAmputatie(afspraakId, amputatie)) {
                        dispatchActions(dispatch, createActionSetVisueleInspectieIcoonPosition(afspraakId, icoonId, x, y));
                    }
                }
            } else {
                switch (aanzicht) {
                    case 'RECHTS_VERTICAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'RECHTERBORST')) {
                            dispatchActions(dispatch, createActionSetSignaleringIcoonPositionRechtsVerticaal(afspraakId, icoonId, x, y));
                        }
                        break;
                    case 'LINKS_VERTICAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'LINKERBORST')) {
                            dispatchActions(dispatch, createActionSetSignaleringIcoonPositionLinksVerticaal(afspraakId, icoonId, x, y));
                        }
                        break;
                    case 'RECHTS_HORIZONTAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'RECHTERBORST')) {
                            dispatchActions(dispatch, createActionSetSignaleringIcoonPositionRechtsHorizontaal(afspraakId, icoonId, x, y));
                        }
                        break;
                    case 'LINKS_HORIZONTAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'LINKERBORST')) {
                            dispatchActions(dispatch, createActionSetSignaleringIcoonPositionLinksHorizontaal(afspraakId, icoonId, x, y));
                        }
                        break;
                    default:
                        console.error('onbekend aanzicht: ' + aanzicht);
                }
            }
        },
        maakIcoon(afspraakId: number, x: number, y: number, type: AnnotatieIcoonType, aanzicht: Aanzicht) {
            if (isVisueleInspectie(parentProps.icoon.type)) {
                const amputatie = x2amputatie(x);
                if (parentProps.icoon.type === 'AMPUTATIE') {
                    if (!verbodenWegensSignaleringsicoon(afspraakId, amputatie)) {
                        setAnnotatieIndienMogelijk(afspraakId, amputatie, dispatch);
                    }
                } else {
                    if (!verbodenWegensAmputatie(afspraakId, amputatie)) {
                        dispatchActions(dispatch, createActionMaakVisueleInspectieIcoon(afspraakId, type, x, y));
                    }
                }
            }
            else {
                switch (aanzicht) {
                    case 'RECHTS_VERTICAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'RECHTERBORST')) {
                            dispatchActions(dispatch, createActionMaakSignaleringIcoonRechtsVerticaal(afspraakId, type, x, y));
                        }
                        break;
                    case 'LINKS_VERTICAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'LINKERBORST')) {
                            dispatchActions(dispatch, createActionMaakSignaleringIcoonLinksVerticaal(afspraakId, type, x, y));
                        }
                        break;
                    case 'RECHTS_HORIZONTAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'RECHTERBORST')) {
                            dispatchActions(dispatch, createActionMaakSignaleringIcoonRechtsHorizontaal(afspraakId, type, x, y));
                        }
                        break;
                    case 'LINKS_HORIZONTAAL':
                        if (!verbodenWegensAmputatie(afspraakId, 'LINKERBORST')) {
                            dispatchActions(dispatch, createActionMaakSignaleringIcoonLinksHorizontaal(afspraakId, type, x, y));
                        }
                        break;
                    default:
                        console.error('onbekend aanzicht: ' + aanzicht);
                }
            }
        },
        verwerkTextChange(afspraakId: number, icoonId: number, newValue: string) {
            dispatchActions(dispatch, createActionSetVisueleInspectieIcoonTekst(afspraakId, icoonId, newValue));
        },
        onMouseDown(afspraakId: number) {
        },
    };
};

function heeftAnnotatieIcoon(afspraakId: number, amputatie: Amputatie) {
    const annotatieAfbeelding: AnnotatieAfbeelding = store.getState().visueleInspectieAfbeeldingByAfspraakId.get(afspraakId);
    if (!annotatieAfbeelding) {
        return null;
    }
    for (let icoon of annotatieAfbeelding.iconenById.values()) {
        if (x2amputatie(icoon.positieX) === amputatie) {
            return icoon;
        }
    }
    return null;
}

function setAnnotatieIndienMogelijk(afspraakId: number, amputatie: Amputatie, dispatch) {
    const annotatieIcoon = heeftAnnotatieIcoon(afspraakId, amputatie);
    if (annotatieIcoon) {
        showErrorToast('De ' + amputatie.toLowerCase() + ' heeft een "' + annotatieIcoon.type.toLowerCase().replace('_', ' ') +
            '" icoon. Verwijder dit icoon voordat je de ' + amputatie.toLowerCase() + ' als geamputeerd markeert.');
    } else {
        dispatchActions(dispatch, createActionSetAmputatie(afspraakId, amputatie));
    }
}

function x2amputatie(x: number) {
    return x < 50 ? 'RECHTERBORST' : 'LINKERBORST';
}

const AnnotatieIcoonContainer = connect(mapStateToProps, mapDispatchToProps)(AnnotatieIcoonView);

export default AnnotatieIcoonContainer;
