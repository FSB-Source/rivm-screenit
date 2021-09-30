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

import type {SeAction} from '../actions/SeAction';

export type TransactionType =
    'INSCHRIJFGEGEVENS_OPSLAAN'
    | 'ONDERZOEK_STARTEN'
    | 'VISUELE_INSPECTIE_OPSLAAN'
    | 'SIGNALEREN_OPSLAAN'
    | 'INSCHRIJVEN_PASSANT'
    | 'BEEINDIGDE_AFSPRAAK_DOORVOEREN'
    | 'UITSCHRIJVEN_CLIENT'
    | 'START_KWALITEITSOPNAME_TRANSACTION'
    | 'BEEINDIG_KWALITEITSOPNAME_TRANSACTION'
    | 'LOG_GEBEURTENIS_SE';

export type Transaction = {
    type: TransactionType,
    clientId: ?number,
    uitnodigingsNr: ?number,
    instellingGebruikerId: number,
    afspraakVanafDatum: string,
    medewerkercode: string,
    sessionId: string,
    actions: Array<SeAction>,
}

export type Action = {
    type: string,
    [key: string]: any
}
