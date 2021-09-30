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

import {createActionClearSession, createActionSetSession} from '../actions/SessionActions';
import {store} from '../Store';
import {leesAfspraken} from './DaglijstRestclient';
import {createActionKiesDaglijstDatum} from '../actions/DaglijstDatumActions';
import {nuTimestamp, vandaagISO} from '../util/DateUtil';
import {baseUrl, createClientHeaders} from '../util/ApiUtil';
import {dismissAllToasts, showErrorToast, showErrorToastWithoutAutoClose, showYubikeyHerkendToast} from '../util/ToastUtil';
import {createActionSetAutorisatie} from '../actions/AutorisatieActions';
import {leesPlanning} from './PlanningRestClient';
import {leesHuisartsen} from './HuisartsenRestclient';
import {logoffToIMS, logonToIMS} from '../util/ImsApiUtil';
import {readMammografen} from './MammografenRestClient';
import {leesZorginstellingen} from './ZorginstellingenRestclient';
import {ensureIdleCheck} from '../util/TimeoutUtil';
import {readSeGebruikers} from './SeGebruikerRestClient';
import {clearWerklijst, getActieveKwaliteitsopname} from './WerklijstRestclient';
import {
    createActionNavigateToConnectiestatus,
    createActionNavigateToDaglijst,
    createActionNavigateToKwaliteitsopname,
    NAVIGATE_TO_KWALITEITSOPNAME,
} from '../actions/NavigationActions';
import {createActionClearCache} from '../actions/ClearCacheActions';
import {getVersie} from './NfcRestClient';
import {aeTitle} from '../util/Util';
import {dispatchActions} from '../util/DispatchUtil';
import {createActionLoginActive, createActionLoginInactive, createActionStopIdentificerenTotYubikeyEraf} from '../actions/LoginStatusActions';
import {readMammografenStatus} from './MammografenStatusRestclient';
import {isAuthorized} from '../util/AutorisatieUtil';

export const GEEN_OTP = 'GEEN_OTP';
export const GEEN_IDENTIFICATIE = 'GEEN_IDENTIFICATIE';
export const YUBIKEY_AFWEZIG = 'YUBIKEY_AFWEZIG';
export const YUBIKEY_ONBEKEND = 'YUBIKEY_ONBEKEND';
export const YUBIKEY_HERKEND = 'YUBIKEY_HERKEND';
export const YUBIKEY_VERLOPEN = 'YUBIKEY_VERLOPEN';

export const login = (username: string, password: string, yubikeyIdentificatie: string, yubikey: string): void => {
    if (store.getState().loginStatus.inlogActief) {
        console.log(nuTimestamp() + ': Skip inlogpoging omdat er al een bezig is');
        return;
    }

    dispatchActions(store.dispatch, createActionLoginActive());
    getVersie().then(function(nfcServerVersie) {
        let loginHeader = new Headers({
            Authorization: `Basic ${new Buffer(`${username}:${password}`).toString('base64')}`,
            yubikeyIdentificatie: yubikeyIdentificatie,
            Yubikey: (yubikey) ? yubikey : GEEN_OTP,
            nfcServerVersie: nfcServerVersie,
        });

        const sessieMeenemen: boolean = username === '' && password === '';
        const meldingTechnischeFout = 'Inloggen mislukt (technische fout)';

        fetch(baseUrl + 'authenticatie/inloggen', {
            method: 'POST',
            headers: loginHeader,
        }).then(function(response) {
            if (response.status === 200) {
                verwerkSuccesvolleLogin(response, yubikeyIdentificatie).then(() => {
                    console.log(nuTimestamp() + ': Klaar met inloggen' + (sessieMeenemen ? ' (sessiemeenemen)' : '') + 'yubikey: ' + yubikeyIdentificatie);
                    dispatchActions(store.dispatch, createActionLoginInactive());
                }).catch(function(error) {
                    mislukteLoginAfronden(meldingTechnischeFout, '(error in verwerkSuccesvolleLogin): ' + error, sessieMeenemen, yubikeyIdentificatie);
                });
            } else if (response.status === 412) {
                mislukteLoginAfronden('SE is niet bekend in ScreenIT, neem contact op met een beheerder.', '(SE onbekend)', sessieMeenemen, yubikeyIdentificatie);
            } else if (response.status === 504) {
                mislukteLoginAfronden('SE is offline, alleen bestaande sessies meenemen mogelijk', '(SE offline)', sessieMeenemen, yubikeyIdentificatie);
            } else {
                response.json().then(error => {
                    const uiBericht = error && error.message ? error.message : standaardFoutMelding;
                    mislukteLoginAfronden(uiBericht, '(Overige error): ' + uiBericht, sessieMeenemen, yubikeyIdentificatie);
                }).catch(function(error) {
                    mislukteLoginAfronden(meldingTechnischeFout, '(error tijdens parse json bij error resonse): ' + error, sessieMeenemen, yubikeyIdentificatie);
                });
            }
        }).catch(function(error) {
            mislukteLoginAfronden(meldingTechnischeFout, '(authenticatieMislukt): ' + error, sessieMeenemen, yubikeyIdentificatie);
        });
    });
};

export const identificeer = (yubikeyIdentificatie: string, yubikey: string): void => {
    if (store.getState().loginStatus.inlogActief) {
        console.log(nuTimestamp() + ': Skip identificeren omdat er login bezig is');
        return;
    }
    fetch(baseUrl + 'authenticatie/identificeren', {
        method: 'POST',
        body: yubikeyIdentificatie,
    }).then(function(response) {
        if (response.status === 200) {
            response.json().then(response => {
                if (response.code === YUBIKEY_AFWEZIG) {
                    dismissAllToasts();
                } else if (response.code === YUBIKEY_ONBEKEND || response.code === YUBIKEY_VERLOPEN) {
                    if (!store.getState().loginStatus.inlogActief) {
                        showYubikeyHerkendToast();
                    }
                } else if (response.code === YUBIKEY_HERKEND) {
                    if (store.getState().session === null) {
                        dismissAllToasts();
                        login('', '', yubikeyIdentificatie, yubikey);
                    }
                }
            });
        }
    }).catch(function(error) {
        showErrorToast('Inloggen mislukt.');
        console.error('Er ontstond een probleem tijdens het uitvoeren van fetch bij identificeren: ' + error.message);
    });
};

export const logout = (yubikeyIdentificatie: string) => {
    if (yubikeyIdentificatie === undefined) {
        logoutClient();
    } else {
        fetch(baseUrl + 'authenticatie/uitloggen', {
            method: 'POST',
            headers: createClientHeaders(),
            body: yubikeyIdentificatie,
        }).then(function() {
            logoutClient();
        }).catch(function(error) {
            logoutClient();
            showErrorToast('Uitloggen mislukt.');
            console.error('Er ontstond een probleem tijdens het uitvoeren van fetch bij uitloggen: ' + error.message);
        });
    }
};

function verwerkSuccesvolleLogin(response: any, yubikeyIdentificatie: string): Promise<any> {
    let token: string = response.headers.get('x-auth-token');
    let autorisatie;
    if (token !== null) {
        return response.json().then(autorisatieObject => {
            ensureIdleCheck();
            autorisatie = autorisatieObject;
            const navigatie = JSON.parse(autorisatie.navigatie);
            store.dispatch(
                createActionSetSession(token, autorisatie.username, autorisatie.medewerkercode, autorisatie.displayName, autorisatie.seCode, autorisatie.seNaam,
                    yubikeyIdentificatie, autorisatie.instellingGebruikerId));
            store.dispatch(createActionSetAutorisatie(autorisatie));
            return navigatie;
        }).then((navigatie) => {
            store.dispatch(createActionKiesDaglijstDatum(vandaagISO()));
            readMammografen().then((mammografen) => {
                if (magAlleenConnectieStatusInzien(autorisatie)) {
                    readMammografenStatus(mammografen).then(() => laadNavigatie(autorisatie, navigatie));
                    return;
                } else if (aeTitle() !== '') {
                    getActieveKwaliteitsopname().then((kwaliteitsopname) => {
                        if (kwaliteitsopname.aeTitle != null) {
                            navigatie = createActionNavigateToKwaliteitsopname();
                        }
                        laadNavigatie(autorisatie, navigatie);
                    }).catch(() => {
                        laadNavigatie(autorisatie, navigatie);
                    });
                } else {
                    if (navigatie.type === NAVIGATE_TO_KWALITEITSOPNAME) {
                        navigatie = createActionNavigateToDaglijst();
                    }
                    laadNavigatie(autorisatie, navigatie);
                }
                readMammografenStatus(mammografen);
            }).catch(() => {
                laadNavigatie(autorisatie, navigatie);
            });
            if (!magAlleenConnectieStatusInzien(autorisatie)) {
                leesHuisartsen();
                leesPlanning(vandaagISO());
                leesZorginstellingen();
                readSeGebruikers();
            }
        });
    } else {
        showErrorToast('Er ging iets fout tijdens het inloggen, probeer het opnieuw of neem contact op met een beheerder');
        console.error('Fout tijdens inloggen: Geen token.');
        return new Promise((resolve => resolve()));
    }
}

const mislukteLoginAfronden = (uiBericht: string, logBericht: string, sessieMeenemen: boolean, yubikeyIdentificatie: string) => {
    let extraLogtekst: string = '';
    if (sessieMeenemen) {
        extraLogtekst = '(sessiemeenemen) ';
        dispatchActions(store.dispatch, createActionStopIdentificerenTotYubikeyEraf());
        console.log('Stop identificeren totdat Yubikey verwijderd wordt: ' + yubikeyIdentificatie);
        const uiTekst = 'Fout tijdens meenemen sessie:\n' + uiBericht;
        showErrorToastWithoutAutoClose(uiTekst);
    } else {
        showErrorToast(uiBericht);
    }
    console.log(nuTimestamp() + ': Klaar met inloggen ' + extraLogtekst + 'voor yubikey: ' + yubikeyIdentificatie + ' ' + logBericht);
    dispatchActions(store.dispatch, createActionLoginInactive());
};

const laadNavigatie = (autorisatie: any, navigatie: any) => {
    if (magAlleenConnectieStatusInzien(autorisatie)) {
        store.dispatch(createActionNavigateToConnectiestatus());
    } else {
        if (Object.keys(navigatie).length === 0) {
            navigatie = createActionNavigateToDaglijst();
        }
        logonIMSEnLeesAfspraken(autorisatie, navigatie);
    }
};

const logonIMSEnLeesAfspraken = (autorisatie: any, navigatie: any = null) => {
    logonToIMS(autorisatie.username);
    leesAfspraken(vandaagISO(), navigatie);
};

export const logoutClient = () => {
    clearWerklijst();
    if (store.getState().session) {
        logoffToIMS(store.getState().session.gebruikersnaam);
    } else {
        console.warn('Logout terwijl er geen sessie is.');
    }
    store.dispatch(createActionClearSession());
    store.dispatch(createActionClearCache());
};

const magAlleenConnectieStatusInzien = (autorisatie: any): boolean => {
    return !isAuthorized(autorisatie.inschrijvenRecht) && !isAuthorized(autorisatie.onderzoekenRecht) && !isAuthorized(autorisatie.signalerenRecht) &&
        !isAuthorized(autorisatie.kwaliteitsopnameRecht) && isAuthorized(autorisatie.connectiestatusRecht);
};
