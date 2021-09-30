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

import {Afspraak} from '../datatypes/Afspraak';
import {store} from '../Store';
import type {Onderzoek} from '../datatypes/Onderzoek';

export const getDagAfspraken = (datum: string): Array<Afspraak> => {
    return [...store.getState().afsprakenById.values()].filter((afspraak: Afspraak) => afspraak.vanafDatum === datum);
};

export const getOnderzoekStatusCount = (status: string, onderzoeken: Map<number, Onderzoek>, afspraken: Map<number, Afspraak>): number => {
    let totalResult = 0;
    afspraken.forEach(function(afspraak) {
        if (status === 'AFGEROND' || status === 'ONDERBROKEN' || status === 'ONVOLLEDIG') {
            if (afspraak.status === 'BEEINDIGD') {
                const onderzoek = onderzoeken.get(afspraak.id);
                if (onderzoek !== undefined && onderzoek.status === status) {
                    totalResult += 1;
                }
            }
        } else {
            if (afspraak.status === status) {
                totalResult += 1;
            }
        }
    });
    return totalResult;
};

export const hasOpenstaandeOnderzoeken = (datum: string) => {
    return getDagAfspraken(datum).some(function(afspraak) {
        return (afspraak.status === 'INGESCHREVEN' || afspraak.status === 'ONDERZOEK' || afspraak.status === 'SIGNALEREN');
    });
};

export const hasNietDoorgevoerdeOnderzoeken = (datum: string) => {
    const dagverslag = store.getState().dagverslag;
    if (dagverslag === undefined || dagverslag.get(datum) === undefined) {
        return false;
    }
    return getDagAfspraken(datum).some(function(afspraak) {
        return (afspraak.status === 'BEEINDIGD' && !afspraak.doorgevoerd);
    });
};
