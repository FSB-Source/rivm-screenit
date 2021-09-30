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
import {toast} from 'react-toastify';

export const MELDING_SESSIE_NIET_GELDIG = 'Uw sessie is niet geldig. Sluit alle ScreenIT vensters, leg Yubikey opnieuw op de lezer en log opnieuw in.';
export const MELDING_TECHNISCHE_FOUT = 'Er is een technische fout opgetreden.';
export const MELDING_DUBBELE_INSTANTIE = 'Er is al een ScreenIT tabblad open, sluit dit tabblad.';
export const MELDING_YUBIKEY_GEDETECTEERD = 'Yubikey gedetecteerd, log in om uw sessie te starten.';
export const MELDING_NFC_ERROR = 'Communicatiefout met Yubikey-lezer. Herstart werkstation.';
export const MELDING_WIJZIGINGEN_VERWERKT = 'Wijzigingen verwerkt.';

export const DEFAULT_TOAST_TIMEOUT = 5000;

let toastIdWijzigingenOpgeslagen = null;
let toastIdYubikeyHerkend = null;
let toastIdError = null;
let toastIdWarning = null;
let toastIdNfcError = null;
const MINUUT: number = 60000;

const dismissToastIfActive = (toastId) => {
    if (toastId !== null && toast.isActive(toastId)) {
        toast.dismiss(toastId);
    }
};

export const dismissYubikeyHerkendToast = () => {
    dismissToastIfActive(toastIdYubikeyHerkend);
};

export const dismissNfcErrorToast = () => {
    dismissToastIfActive(toastIdNfcError);
};

export const dismissAllToasts = () => {
    dismissToastIfActive(toastIdWijzigingenOpgeslagen);
    dismissYubikeyHerkendToast();
    dismissNfcErrorToast();
    dismissToastIfActive(toastIdError);
    dismissToastIfActive(toastIdWarning);
};

export const showWijzigingenOpgeslagenToast = () => {
    if (!toast.isActive(toastIdWijzigingenOpgeslagen)) {
        toastIdWijzigingenOpgeslagen = toast.success(MELDING_WIJZIGINGEN_VERWERKT, {className: 'success-color'});
    }
};

export const showYubikeyHerkendToast = () => {
    if (!toast.isActive(toastIdYubikeyHerkend)) {
        toastIdYubikeyHerkend = toast.info(MELDING_YUBIKEY_GEDETECTEERD, {className: 'info-color', autoClose: false});
    }
};

export const showNfcErrorToast = () => {
    dismissYubikeyHerkendToast();
    if (!toast.isActive(toastIdNfcError)) {
        toastIdNfcError = toast.error(MELDING_NFC_ERROR, {className: 'danger-color', autoClose: false});
    }
};

export const showWarningToast = (message: string) => {
    if (!toast.isActive(toastIdWarning)) {
        toastIdWarning = toast.warn(message, {autoClose: MINUUT});
    }
};

export const showErrorToast = (message: string) => {
    if (!toast.isActive(toastIdError)) {
        toastIdError = toast.error(message, {className: 'danger-color'});
    }
};

export const showErrorToastWithoutAutoClose = (message: string) => {
    if (!toast.isActive(toastIdError)) {
        toastIdError = toast.error(message, {className: 'danger-color', autoClose: false});
    }
};

export const persistentErrorToast = (message: string) => {
    toast.error(message, {className: 'danger-color'});
};

export const persistentSuccessToast = (message: string) => {
    toast.success(message, {className: 'success-color'});
};
