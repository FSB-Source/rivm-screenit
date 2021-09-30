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

export type OnvolledigOnderzoekOption = 'MET_FOTOS' | 'ZONDER_FOTOS';

export const allOnvolledigOnderzoekOptions: Array<OnvolledigOnderzoekOption> = ['MET_FOTOS', 'ZONDER_FOTOS'];

export const getOnvolledigOnderzoekLabel = (option: OnvolledigOnderzoekOption): string => {
    switch (option) {
        case 'MET_FOTOS':
            return 'Met foto\'s';
        case 'ZONDER_FOTOS':
            return 'Zonder foto\'s';
        default:
            return '';
    }
};
