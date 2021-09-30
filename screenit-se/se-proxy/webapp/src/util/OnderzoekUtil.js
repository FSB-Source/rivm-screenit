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
import type {Client} from '../datatypes/Client';
import {createActionMaakDubbeleTijd, createActionMaakDubbeleTijdReden} from '../actions/AanvullendeInformatieActions';
import type {SeAction} from '../actions/SeAction';

export const createDubbeleTijdActions = (afspraak: Afspraak, client: Client): Array<SeAction> => {
    const result: Array<SeAction> = [];
    result.push(createActionMaakDubbeleTijd(afspraak.id, client.id, client.doelgroep === 'DUBBELE_TIJD'));
    result.push(createActionMaakDubbeleTijdReden(afspraak.id, client.id, client.doelgroep === 'DUBBELE_TIJD' ? client.dubbeleTijdReden : null));
    return result;
};
