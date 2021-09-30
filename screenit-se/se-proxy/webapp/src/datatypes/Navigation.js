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

export type Tab = 'Daglijst' | 'Cliëntgegevens' | 'Onderzoek' | 'Dagverslag' | 'Kwaliteitsopname' | 'Connectiestatus' | 'Geen';
export type SubPagina = 'Vorige onderzoeken' | 'Visuele inspectie' | 'Signaleren';

export type NavigationState = {
    +tab: Tab,
    +subPagina: SubPagina | null,
    +clientId: number | null,
    +afspraakId: number | null
};

export const isDaglijstTab = (tab: Tab): boolean => {
    return tab === ('Daglijst': Tab);
};

export const isClientgegevensTab = (tab: Tab): boolean => {
    return tab === ('Cliëntgegevens': Tab);
};

export const isOnderzoeksTab = (tab: Tab): boolean => {
    return tab === ('Onderzoek': Tab);
};

export const isDagverslagTab = (tab: Tab): boolean => {
    return tab === ('Dagverslag': Tab);
};

export const isKwaliteitsopnameTab = (tab: Tab): boolean => {
    return tab === ('Kwaliteitsopname': Tab);
};

export const isConnectiestatusTab = (tab: Tab): boolean => {
    return tab === ('Connectiestatus': Tab);
};
