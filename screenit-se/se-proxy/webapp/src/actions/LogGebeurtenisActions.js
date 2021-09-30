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

export type LogGebeurtenisActions =
    LogGebeurtenisBeeldenVorigeRondeOpgehaaldAction
    | LogGebeurtenisBeeldenAnnotatieAmputatieMismatchAction
    | LogGebeurtenisBeeldenGeenMppsOntvangenAction;

export const LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD = 'LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD';
export type LogGebeurtenisBeeldenVorigeRondeOpgehaaldAction = { type: 'LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD', logMessage: string }
export const createActionLogGebeurtenisBeeldenVorigeRondeOpgehaald = (): LogGebeurtenisBeeldenVorigeRondeOpgehaaldAction => {
    return {
        type: LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD,
        logMessage: 'Er zijn beelden opgehaald uit vorige ronde.',
    };
};

export const LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH = 'LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH';
export type LogGebeurtenisBeeldenAnnotatieAmputatieMismatchAction = { type: 'LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH', logMessage: string }
export const createActionLogGebeurtenisBeeldenAnnotatieAmputatieMismatch = (): LogGebeurtenisBeeldenAnnotatieAmputatieMismatchAction => {
    return {
        type: LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH,
        logMessage: 'Verschil tussen zijde beelden en zijde amputatiekruis, gebruiker heeft geaccordeerd.',
    };
};

export const LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN = 'LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN';
export type LogGebeurtenisBeeldenGeenMppsOntvangenAction = { type: 'LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN', logMessage: string }
export const createActionLogGebeurtenisBeeldenGeenMppsOntvangen = (): LogGebeurtenisBeeldenGeenMppsOntvangenAction => {
    return {
        type: LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN,
        logMessage: 'Er is een waarschuwing getoond voor het ontbreken van een MPPS bericht.',
    };
};
