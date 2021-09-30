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
import {
    createActionNavigateToClientgegevens,
    createActionNavigateToConnectiestatus,
    createActionNavigateToDaglijst,
    createActionNavigateToDagverslag,
    createActionNavigateToKwaliteitsopname,
    createActionNavigateToOnderzoek,
} from '../../actions/NavigationActions';
import {leesAfspraken} from '../../restclient/DaglijstRestclient';
import TabbarView from './TabbarView';
import type {SubPagina, Tab} from '../../datatypes/Navigation';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {State} from '../../datatypes/State';
import {getIfExists} from '../../util/MapUtil';
import {showErrorToast} from '../../util/ToastUtil';
import {store} from '../../Store';
import {getDagverslag} from '../../restclient/DagverslagRestClient';
import {datumInVerleden} from '../../util/DateUtil';
import {hasNietDoorgevoerdeOnderzoeken, hasOpenstaandeOnderzoeken} from '../../util/AfsprakenUtil';
import React from 'react';
import {createActionWijzigingenVerwerkt} from '../../actions/WijzigingenActions';
import {showWijzigingenPopup} from '../../util/PopupUtil';
import {createActionDeleteVisueleInspectieAfbeeldingByAfspraakId} from '../../actions/VisueleInspectieActions';

const mapStateToProps = (state: State) => {
    const afspraak: Afspraak | null = getIfExists(state.afsprakenById, state.navigation.afspraakId);
    const activeTab: Tab = state.navigation.tab;
    const daglijstTabClickable = (!datumInVerleden(state.daglijstDatum)
        || hasOpenstaandeOnderzoeken(state.daglijstDatum)
        || hasNietDoorgevoerdeOnderzoeken(state.daglijstDatum)) && !state.bezigMetKwaliteitsopnameVolgnr && state.autorisatie.inschrijven;
    return {
        activeTab: activeTab,
        afspraak: afspraak,
        daglijstTabClickable: daglijstTabClickable,
        clientgegevensTabClickable: activeTab === 'Onderzoek',
        onderzoekTabClickable: activeTab === 'Cliëntgegevens' && afspraak && afspraak.status !== 'VERWACHT' && state.autorisatie.onderzoeken,
        dagverslagTabClickable: activeTab === 'Daglijst' && state.online,
        kwaliteitsopnameTabClickable: state.autorisatie.kwaliteitsopname && !datumInVerleden(state.daglijstDatum) && state.huidigeMammograafId && state.online,
        connectiestatusTabClickable: state.autorisatie.connectiestatus,
        online: state.online,
        subPagina: state.navigation.subPagina,
        magOnderzoeken: state.autorisatie.onderzoeken,
        daglijstDatum: state.daglijstDatum,
        error: state.error,
        huidigeMammograafId: state.huidigeMammograafId,
        bezigMetKwaliteitsopname: state.bezigMetKwaliteitsopnameVolgnr,
        klaarMetDataLaden: state.opgehaaldeDagen.has(state.daglijstDatum),
    };
};

const mapDispatchToProps = dispatch => ({
    onClickTab(tab: Tab, subPagina: ?SubPagina, afspraak: Afspraak, magOnderzoeken: boolean): void {
        if (store.getState().heeftWijzigingen) {
            showWijzigingenPopup(() => {
                leesAfspraken(store.getState().daglijstDatum);
                if (subPagina && subPagina === 'Visuele inspectie') {
                    dispatch(createActionDeleteVisueleInspectieAfbeeldingByAfspraakId(afspraak.id));
                }
                dispatch(createActionWijzigingenVerwerkt());
                navigeerNaarTab(dispatch, tab, afspraak, magOnderzoeken, this);
            }, dispatch);
        } else {
            navigeerNaarTab(dispatch, tab, afspraak, magOnderzoeken, this);
        }
    },
});

const navigeerNaarTab = (dispatch: any, tab: Tab, afspraak: Afspraak, magOnderzoeken: boolean, props: any) => {
    switch (tab) {
        case 'Daglijst':
            if (props.daglijstTabClickable) {
                dispatch(createActionNavigateToDaglijst());
            } else if (props.bezigMetKwaliteitsopnameVolgnr) {
                showErrorToast('Beëindig de kwaliteitsopname voordat u naar de daglijst navigeert');
            }
            break;
        case 'Cliëntgegevens':
            if (afspraak) {
                dispatch(createActionNavigateToClientgegevens(afspraak.clientId, afspraak.id));
            } else {
                showErrorToast('Klik in de daglijst op een afspraak om de cliëntgegevens te zien.');
            }
            break;
        case 'Onderzoek':
            if (magOnderzoeken) {
                if (afspraak) {
                    switch (afspraak.status) {
                        case 'VERWACHT':
                            showErrorToast('Schrijf de client eerst in.');
                            break;
                        case 'INGESCHREVEN':
                            dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Vorige onderzoeken'));
                            break;
                        case 'ONDERZOEK':
                            dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Visuele inspectie'));
                            break;
                        case 'SIGNALEREN':
                            dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Signaleren'));
                            break;
                        case 'BEEINDIGD':
                            dispatch(createActionNavigateToOnderzoek(afspraak.clientId, afspraak.id, 'Signaleren'));
                            break;
                        case 'KWALITEITSOPNAME':
                            dispatch(createActionNavigateToKwaliteitsopname());
                            break;
                        default:
                            break;
                    }
                } else {
                    showErrorToast('Klik in de daglijst op een ingeschreven client om het onderzoek te doen, of schrijf een client in.');
                }
            } else {
                showErrorToast('Hiervoor heeft u niet de benodigde autorisatie: Onderzoek starten op SE.');
            }
            break;
        case 'Dagverslag':
            if (props.dagverslagTabClickable) {
                getDagverslag(store.getState().daglijstDatum).then(function() {
                    store.dispatch(createActionNavigateToDagverslag());
                });
            } else if (!props.online) {
                showErrorToast('Het dagverslag is niet inzichtelijk wanneer de SE offline is.');
            } else {
                showErrorToast('Het dagverslag is alleen toegankelijk vanuit de daglijst.');
            }
            break;
        case 'Kwaliteitsopname':
            if (props.kwaliteitsopnameTabClickable) {
                store.dispatch(createActionNavigateToKwaliteitsopname());
            } else if (!props.huidigeMammograafId) {
                showErrorToast('Kwaliteitsopname is niet mogelijk omdat er aan dit werkstation geen mammograaf gekoppeld is.');
            } else if (datumInVerleden(props.daglijstDatum)) {
                showErrorToast('Kwaliteitsopname is niet mogelijk omdat de datum van daglijst en dagverslag in het verleden ligt.');
            } else if (!props.online) {
                showErrorToast('Kwaliteitsopname is niet mogelijk wanneer de SE offline is.');
            } else {
                showErrorToast('U heeft geen autorisatie voor de kwaliteitsopname.');
            }
            break;
        case 'Connectiestatus':
            if (props.connectiestatusTabClickable) {
                store.dispatch(createActionNavigateToConnectiestatus());
            } else {
                showErrorToast('U heeft geen autorisatie om de connectiestatus in te zien.');
            }
            break;
        default:
            break;
    }
};

const TabbarContainer = connect(mapStateToProps, mapDispatchToProps)(TabbarView);

export default TabbarContainer;
