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

export type ExtraFotosReden =
    'OMVANG_MAMMAE'
    | 'TEPELPROJECTIE'
    | 'KLIERWEEFSEL_NIET_VOLLEDIG_AFGEBEELD'
    | 'INSTELTECHNISCHE_REDEN'
    | 'INTERPRETATIE_KAN_PROBLEMEN_OPLEVEREN'
    | 'ARCHITECTUUR_VERSTORING'
    | 'ASYMMETRIE'
    | 'BEWOGEN_FOTO'
    | 'TECHNISCHE_REDEN'
    | 'WETENSCHAPPELIJK_ONDERZOEK'
    | 'OVERIGE_REDEN';

export const getExtraFotosRedenLabel = (option: ExtraFotosReden): string => {
    switch (option) {
        case 'OMVANG_MAMMAE':
            return 'Omvang mammae';
        case 'TEPELPROJECTIE':
            return 'Tepelprojectie';
        case 'KLIERWEEFSEL_NIET_VOLLEDIG_AFGEBEELD':
            return 'Klierweefsel niet volledig afgebeeld';
        case 'INSTELTECHNISCHE_REDEN':
            return 'Insteltechnische reden';
        case 'INTERPRETATIE_KAN_PROBLEMEN_OPLEVEREN':
            return 'Interpretatie kan problemen opleveren';
        case 'ARCHITECTUUR_VERSTORING':
            return 'Architectuur verstoring';
        case 'ASYMMETRIE':
            return 'Asymmetrie';
        case 'BEWOGEN_FOTO':
            return 'Bewogen foto';
        case 'TECHNISCHE_REDEN':
            return 'Technische reden';
        case 'WETENSCHAPPELIJK_ONDERZOEK':
            return 'Wetenschappelijk Onderzoek';
        case 'OVERIGE_REDEN':
            return 'Overige reden';
        default:
            return '';
    }
};

export const allExtraFotosRedenOptions: Array<ExtraFotosReden> = [
    'OMVANG_MAMMAE'
    , 'TEPELPROJECTIE'
    , 'KLIERWEEFSEL_NIET_VOLLEDIG_AFGEBEELD'
    , 'INSTELTECHNISCHE_REDEN'
    , 'INTERPRETATIE_KAN_PROBLEMEN_OPLEVEREN'
    , 'ARCHITECTUUR_VERSTORING'
    , 'ASYMMETRIE'
    , 'BEWOGEN_FOTO'
    , 'TECHNISCHE_REDEN'
    , 'WETENSCHAPPELIJK_ONDERZOEK'
    , 'OVERIGE_REDEN'];
