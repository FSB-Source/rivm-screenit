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
import OnderzoekView from './OnderzoekView';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {Client} from '../../datatypes/Client';
import type {ClientWerklijstItem} from '../../datatypes/ClientWerklijstItem';
import type {State} from '../../datatypes/State';
import {getMandatory} from '../../util/MapUtil';
import type {Amputatie, Onderzoek} from '../../datatypes/Onderzoek';
import {store} from '../../Store';
import {showErrorToast} from '../../util/ToastUtil';
import type {Signalering} from '../../datatypes/Signalering';
import type {DoorsnedeAfbeeldingen} from '../../datatypes/DoorsnedeAfbeeldingen';
import type {AnnotatieAfbeelding} from '../../datatypes/AnnotatieAfbeelding';

const mapStateToProps = (state: State) => {
    const client: Client = getMandatory(state.clientenById, state.navigation.clientId);
    const afspraak: Afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId);

    const studyToShowOnIms: number = afspraak.uitnodigingsNr;

    return {
        client: getMandatory(state.clientenById, state.navigation.clientId),
        afspraak: getMandatory(state.afsprakenById, state.navigation.afspraakId),
        subPagina: state.navigation.subPagina,
        gebruikersnaam: state.session.gebruikersnaam,
        studyForIms: studyToShowOnIms,
        activeStudyForIms: state.activeStudyForIms,
    };
};

const mapDispatchToProps = () => {
    return {};
};

export const maakWerklijstItem = (afspraak: Afspraak, client: Client, aeTitle: string): ClientWerklijstItem => {
    return {
        bsn: client.bsn,
        uitnodigingsNr: afspraak.uitnodigingsNr,
        geboortedatum: client.geboortedatum,
        voorletters: client.voorletters,
        tussenvoegsel: client.geboorteTussenvoegsel,
        achternaam: client.geboorteAchternaam,
        startDatumTijd: afspraak.vanafDatum + 'T' + afspraak.vanafTijd,
        geslacht: client.geslacht,
        aeTitle,
        medewerkercode: store.getState().session.medewerkercode,
    };
};

const OnderzoekContainer = connect(mapStateToProps, mapDispatchToProps)(OnderzoekView);

export const verbodenWegensAmputatie = (afspraakId: number, amputatie: Amputatie): boolean => {
    const onderzoek: Onderzoek = getMandatory(store.getState().onderzoekByAfspraakId, afspraakId);
    const result: boolean = onderzoek.amputatie === amputatie;
    if (result) {
        showErrorToast('De ' + amputatie.toLowerCase() + ' is gemarkeerd als geamputeerd, daarom is het niet mogelijk er een ander icoon op te plaatsen.');
    }
    return result;
};

export const verbodenWegensSignaleringsicoon = (afspraakId: number, amputatie: Amputatie): boolean => {
    const signalering: ?Signalering = store.getState().signaleringByAfspraakId.get(afspraakId);
    if (!signalering) {
        return false;
    }
    const afbeeldingen: DoorsnedeAfbeeldingen = signalering.doorsnedeAfbeeldingen;
    const result: boolean = amputatie === 'RECHTERBORST' && (heeftIcoon(afbeeldingen.rechtsVerticaleDoorsnede) || heeftIcoon((afbeeldingen.rechtsHorizontaleDoorsnede)))
        || amputatie === 'LINKERBORST' && (heeftIcoon(afbeeldingen.linksVerticaleDoorsnede) || heeftIcoon((afbeeldingen.linksHorizontaleDoorsnede)));
    if (result) {
        showErrorToast('Het is niet mogelijk om deze borst als geamputeerd te markeren. Bij het signaleren zijn reeds annotaties geplaatst, verwijder deze indien nodig.');
    }
    return result;
};

const heeftIcoon = (doorsnede: ?AnnotatieAfbeelding): boolean => {
    return doorsnede !== null && doorsnede !== undefined && doorsnede.iconenById.size > 0;
};

export default OnderzoekContainer;
