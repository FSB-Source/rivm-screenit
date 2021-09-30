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

import {connect} from 'react-redux';
import type {Client} from '../../datatypes/Client';
import type {State} from '../../datatypes/State';
import {getIfExists, getMandatory} from '../../util/MapUtil';
import type {Planning} from '../../datatypes/Planning';
import {Tijdslot} from '../../datatypes/Planning';
import {Afspraak} from '../../datatypes/Afspraak';
import type {Onderzoek, Onderzoekstatus} from '../../datatypes/Onderzoek';
import AfspraakOverzichtView from './AfspraakOverzichtView';

const mapStateToProps = (state: State) => {
    const planning: ?Planning = getIfExists(state.planning, state.daglijstDatum);
    const huidigeDagAfspraken: Array<Afspraak> = [...state.afsprakenById.values()].filter((afspraak: Afspraak) => afspraak.vanafDatum === state.daglijstDatum);
    const geplandeAfspraken: Array<Afspraak> = huidigeDagAfspraken.filter((afspraak: Afspraak) => afspraak.status !== 'BEEINDIGD');
    const nietAfgerondeTijdSlots: Array<Tijdslot> = [];
    nietAfgerondeTijdSlots.push(...geplandeAfspraken);
    if (planning) {
        nietAfgerondeTijdSlots.push(...planning.geenScreeningBlokken.filter(geenScreeningblok => new Date(geenScreeningblok.totDatumTijd) >= new Date()));
    }
    const sortedNietAfgerondeTijdSlots: Array<Tijdslot> = nietAfgerondeTijdSlots.sort((a: Tijdslot, b: Tijdslot) => {
        if (a.vanafTijd > b.vanafTijd) {
            return 1;
        } else if (a.vanafTijd < b.vanafTijd) {
            return -1;
        }
        if (a instanceof Afspraak && b instanceof Afspraak) {
            const clientA: Client = getMandatory(state.clientenById, a.clientId);
            const clientB: Client = getMandatory(state.clientenById, b.clientId);
            return clientA.bsn > clientB.bsn ? 1 : -1;
        }
        return 0;
    });

    const sortedAfgerondeTijdSlots: Array<Tijdslot> = [];
    sortedAfgerondeTijdSlots.push(...huidigeDagAfspraken.filter(
        (afspraak: Afspraak) => afspraak.status === 'BEEINDIGD' && heeftAfgerondStatus(state, afspraak.id, 'ONDERBROKEN')).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1));
    sortedAfgerondeTijdSlots.push(...huidigeDagAfspraken.filter(
        (afspraak: Afspraak) => afspraak.status === 'BEEINDIGD' && heeftAfgerondStatus(state, afspraak.id, 'ONVOLLEDIG')).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1));
    sortedAfgerondeTijdSlots.push(...huidigeDagAfspraken.filter(
        (afspraak: Afspraak) => afspraak.status === 'BEEINDIGD' && heeftAfgerondStatus(state, afspraak.id, 'AFGEROND')).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1));

    if (planning) {
        sortedAfgerondeTijdSlots.push(
            ...planning.geenScreeningBlokken.filter(geenScreeningblok => new Date(geenScreeningblok.totDatumTijd) < new Date()).sort((a, b) => a.vanafTijd > b.vanafTijd ? 1 : -1));
    }

    return {
        nietAfgerondeTijdSlots: sortedNietAfgerondeTijdSlots,
        afgerondeTijdSlots: sortedAfgerondeTijdSlots,
        clienten: state.clientenById,
        daglijstDatum: state.daglijstDatum,
    };
};

const heeftAfgerondStatus = (state: State, afspraakId: number, status: Onderzoekstatus): boolean => {
    const onderzoek: Onderzoek | null = getIfExists(state.onderzoekByAfspraakId, afspraakId);
    return onderzoek !== null && onderzoek.status === status;
};

const mapDispatchToProps = () => {
    return {};
};

const AfspraakOverzichtContainer = connect(mapStateToProps, mapDispatchToProps)(AfspraakOverzichtView);

export default AfspraakOverzichtContainer;
