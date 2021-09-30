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

import type {Afspraak} from './Afspraak';
import type {Client} from './Client';
import type {NavigationState} from './Navigation';
import type {Session} from './Session';
import type {EnvironmentInfo} from './EnvironmentInfo';
import type {ErrorDto} from './ErrorDto';
import type {Recht} from './Recht';
import type {AnnotatieAfbeelding} from './AnnotatieAfbeelding';
import type {Form, FORM_ID} from './Form';
import type {Onderzoek} from './Onderzoek';
import type {Planning} from './Planning';
import type {Signalering} from './Signalering';
import type {Huisarts} from './Huisarts';
import {Mammograaf} from './Mammograaf';
import type {Zorginstelling} from './Zorginstelling';
import type {Popup} from './Popup';
import type {Dagverslag} from './Dagverslag';
import type {ConnectieStatus} from './connectiestatus/ConnectieStatus';
import type {MammografenStatus} from './connectiestatus/MammografenStatus';
import type {LoginStatus} from './LoginStatus';

export type State = {
    +afsprakenById: Map<number, Afspraak>,
    +daglijstDatum: string,
    +seGebruikers: Map<number, string>,
    +clientenById: Map<number, Client>,
    +dagverslag: Map<string, Dagverslag>,
    +session: Session,
    +nietAfgeslotenVanaf: string,
    +planning: Map<string, Planning>, 
    +visueleInspectieAfbeeldingByAfspraakId: Map<number, AnnotatieAfbeelding>,
    +navigation: NavigationState,
    +formsByFormId: Map<FORM_ID, Form>,
    +environmentInfo: EnvironmentInfo,
    +autorisatie: Recht,
    +onderzoekByAfspraakId: Map<number, Onderzoek>,
    +signaleringByAfspraakId: Map<number, Signalering>,
    +huisartsenById: Map<number, Huisarts>,
    +zorginstellingen: Map<number, Zorginstelling>,
    +error: ErrorDto | null,
    +mammografenById: Map<number, Mammograaf>,
    +huidigeMammograafId?: number, 

    +popup: Popup,
    +online: boolean,
    +heeftWijzigingen: boolean,
    +activeStudyForIms: ?number,   
    +bezigMetKwaliteitsopnameVolgnr: ?number, 
    +dubbeleInstantie: boolean,
    +loginStatus: LoginStatus,
    +opgehaaldeDagen: Set<string>,
    +connectieStatus: ConnectieStatus,
    +mammografenStatus: MammografenStatus, 
}
