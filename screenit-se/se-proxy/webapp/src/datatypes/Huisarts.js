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

export type HuisartsDto = {
    id: number;
    naamHuisarts: string;
    praktijknaam: string;
    type: string;
    postcode: string;
    huisnummerVolledig: string;
    straatnaam: string;
    plaats: string;
    weergaveNaam: string;
}

export type Huisarts = HuisartsDto;

export type GeenHuisartsOption = 'HUISARTS_IN_HET_BUITENLAND' | 'CLIENT_WIL_HUISARTS_NIET_OPGEVEN' | 'TEHUIS_HUISARTS' | 'HUISARTS_STAAT_ER_NIET_TUSSEN';

export const allGeenHuisartsOptions: Array<GeenHuisartsOption> = [
    'HUISARTS_IN_HET_BUITENLAND',
    'CLIENT_WIL_HUISARTS_NIET_OPGEVEN',
    'TEHUIS_HUISARTS',
    'HUISARTS_STAAT_ER_NIET_TUSSEN'];

export const getGeenHuisartsLabel = (option: ?GeenHuisartsOption): string => {
    switch (option) {
        case 'HUISARTS_IN_HET_BUITENLAND':
            return 'Huisarts in het buitenland';
        case 'CLIENT_WIL_HUISARTS_NIET_OPGEVEN':
            return 'Cliënt heeft geen huisarts';
        case 'TEHUIS_HUISARTS':
            return 'Tehuis huisarts';
        case 'HUISARTS_STAAT_ER_NIET_TUSSEN':
            return 'Huisarts staat er niet tussen';
        default:
            return '';
    }
};

export const getHuisartsVolledigAdres = (huisarts: Huisarts): string => {
    let adresVolledig: string = huisarts.straatnaam ? huisarts.straatnaam : '';
    adresVolledig.length > 0 && (adresVolledig += ' ');
    adresVolledig += huisarts.huisnummerVolledig ? huisarts.huisnummerVolledig : '';
    adresVolledig.length > 0 && (adresVolledig += ', ');
    adresVolledig += huisarts.postcode ? huisarts.postcode : '';
    huisarts.postcode && huisarts.postcode.length > 0 && (adresVolledig += ' ');
    adresVolledig += huisarts.plaats ? huisarts.plaats : '';
    return adresVolledig;
};
