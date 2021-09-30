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

import {store} from '../Store';
import {createActionClearAfspraken, createActionVulAfspraken} from '../actions/AfspraakActions';
import {createActionVulClienten} from '../actions/ClientActions';
import type {AfspraakDto} from '../datatypes/Afspraak';
import {Afspraak} from '../datatypes/Afspraak';
import type {ClientDto} from '../datatypes/Client';
import {mapClientDtoToClient} from '../datatypes/Client';
import {fetchApi} from '../util/ApiUtil';
import {createActionVulOnderzoekByAfspraakId} from '../actions/OnderzoekActions';
import type {AnnotatieAfbeelding} from '../datatypes/AnnotatieAfbeelding';
import {mapAfbeeldingDtoToAfbeelding} from '../datatypes/AnnotatieAfbeelding';
import type {MammografieDto} from '../datatypes/Mammografie';
import {createActionVulVisueleInspectieAfbeeldingenByAfspraakId} from '../actions/VisueleInspectieActions';
import {createActionVulSignaleringByAfspraakId} from '../actions/SignalerenActions';
import {createActionNavigateToDaglijst, createActionNavigationRestore} from '../actions/NavigationActions';
import {dismissAllToasts, showErrorToast} from '../util/ToastUtil';
import {leesPlanning} from './PlanningRestClient';
import {getDate, getTime, vandaagISO} from '../util/DateUtil';
import {getDagverslag} from './DagverslagRestClient';
import type {Transaction} from '../datatypes/Transaction';
import {dispatchActions} from '../util/DispatchUtil';
import {createActionDaglijstOpgehaald} from '../actions/OpgehaaldeDagenActions';

const clientFromDto = (clientDto: ClientDto) => mapClientDtoToClient(clientDto);

const afspraakFromDto = (afspraakDto: AfspraakDto): Afspraak => {
    const afspraak: Afspraak = new Afspraak();
    afspraak.id = afspraakDto.id;
    afspraak.vanafDatum = getDate(afspraakDto.vanaf);
    afspraak.vanafTijd = getTime(afspraakDto.vanaf);
    afspraak.status = afspraakDto.status;
    afspraak.onderzoekId = afspraakDto.huidigOnderzoek ? afspraakDto.huidigOnderzoek.id : null;
    afspraak.clientId = afspraakDto.client.id;
    afspraak.bezwaarDoorgevroerdOpCentraal = !!afspraakDto.bezwaarAangevraagd;
    afspraak.bezwaarAangevraagd = afspraakDto.bezwaarAangevraagd;
    afspraak.identificatiesoort = afspraakDto.identificatiesoort;
    afspraak.identificatienummer = afspraakDto.identificatienummer;
    afspraak.aantalOpgekomen = afspraakDto.aantalOpgekomen;
    afspraak.aantalOproepen = afspraakDto.aantalOproepen;
    afspraak.uitnodigingsNr = afspraakDto.uitnodigingsNr;
    afspraak.huisartsId = afspraakDto.huisartsId;
    afspraak.geenHuisartsOptie = afspraakDto.geenHuisartsOptie;
    afspraak.doorgevoerd = afspraakDto.doorgevoerd;
    afspraak.centralAvailable = afspraakDto.centralAvailable;
    afspraak.eerderOnderbrokenInZelfdeRonde = afspraakDto.eerderOnderbrokenInZelfdeRonde;
    afspraak.eerdereOpschortenReden = afspraakDto.eerdereOpschortenReden;
    afspraak.eerdereOpschortenRedenTekst = afspraakDto.eerdereOpschortenRedenTekst;
    afspraak.geforceerd = afspraakDto.geforceerd;

    return afspraak;
};

const visueleInspectieAfbeeldingFromDto = (afspraakId: number, mammografieDto: MammografieDto): AnnotatieAfbeelding => {
    return mapAfbeeldingDtoToAfbeelding(afspraakId, mammografieDto.visueleInspectieAfbeelding);
};

export const GEFORCEERD_DAGLIJST_OPHALEN = true;
export const leesAfspraken = (datum: string, navigatie: any = null, geforceerd: boolean = false): void => {
    fetchApi('GET', 'daglijst/' + (geforceerd ? 'geforceerd/' : '') + datum,
        (daglijstMetMutaties) => {
            vulAfspraken(datum, navigatie, JSON.parse(daglijstMetMutaties.daglijstJson), daglijstMetMutaties.mutatieJsons);
            dispatchActions(store.dispatch, createActionDaglijstOpgehaald(datum));
        },
    );
    if (store.getState().online) {
        getDagverslag(datum);
    }
};

export const vulAfspraken = (datum: string, navigatie: any = null, afspraakDtos: Array<AfspraakDto>, mutatiesJson: Array<string>) => {
    if (!afspraakDtos || (afspraakDtos: any).errorReferentie) {
        showErrorToast('Daglijst en/of dagverslag niet beschikbaar.');
        return;
    }
    new Promise(function(resolve) {
        store.dispatch(createActionClearAfspraken(datum));
        store.dispatch(createActionVulClienten(afspraakDtos.map((afspraakDto: AfspraakDto) => clientFromDto(afspraakDto.client))));
        store.dispatch(createActionVulAfspraken(afspraakDtos.map((afspraakDto: AfspraakDto) => afspraakFromDto(afspraakDto))));
        store.dispatch(createActionVulVisueleInspectieAfbeeldingenByAfspraakId(
            afspraakDtos.filter((afspraakDto: AfspraakDto) => afspraakDto.mammografie != null && afspraakDto.mammografie.visueleInspectieAfbeelding != null).
                map((afspraakDto: AfspraakDto) => visueleInspectieAfbeeldingFromDto(afspraakDto.id, afspraakDto.mammografie))));
        store.dispatch(createActionVulOnderzoekByAfspraakId(afspraakDtos));
        store.dispatch(createActionVulSignaleringByAfspraakId(afspraakDtos));
        mutatiesJson.forEach(mutatie => {
            const receivedTransaction: Transaction = JSON.parse(mutatie);
            dispatchActions(store.dispatch, ...receivedTransaction.actions);
        });
        dismissAllToasts();
        resolve();
    }).then(function() {
        if (navigatie !== undefined && navigatie !== null && navigatie.type !== null && !isEmpty(navigatie)) {
            store.dispatch(createActionNavigationRestore(navigatie.type, navigatie.clientId, navigatie.afspraakId, navigatie.subPagina));
        }
    });
};

export const vernieuwAfsprakenDaglijst = (geforceerd: boolean = false) => {
    store.dispatch(createActionClearAfspraken(vandaagISO()));
    const leesGeforceerd: boolean = geforceerd && store.getState().online;
    leesAfspraken(vandaagISO(), createActionNavigateToDaglijst(), leesGeforceerd);
    leesPlanning(vandaagISO());
};

function isEmpty(obj) {
    for (var key in obj) {
        if (obj.hasOwnProperty(key)) return false;
    }
    return true;
}
