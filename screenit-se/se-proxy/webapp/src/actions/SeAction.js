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

import type {AanvullendeInformatieActions} from './AanvullendeInformatieActions';
import type {MBBSignaleringActions} from './MBBSignaleringActions';
import type {OnderzoekActions} from './OnderzoekActions';
import type {AfspraakActions} from './AfspraakActions';
import type {AutorisatieActions} from './AutorisatieActions';
import type {ClientActions} from './ClientActions';
import type {DaglijstDatumActions} from './DaglijstDatumActions';
import type {DagverslagActions} from './DagverslagActions';
import type {EnvironmentInfoActions} from './EnvironmentInfoActions';
import type {ErrorActions} from './ErrorActions';
import type {FormActions} from './FormActions';
import type {HuisartsActions} from './HuisartsActions';
import type {LogGebeurtenisActions} from './LogGebeurtenisActions';
import type {MammograafActions} from './MammograafActions';
import type {NavigationActions} from './NavigationActions';
import type {WijzigingenActions} from './WijzigingenActions';
import type {PassantAfspraakMakenActions} from './PassantAfspraakMakenActions';
import type {PlanningActions} from './PlanningActions';
import type {PopupActions} from './PopupActions';
import type {SeGebruikersActions} from './SeGebruikersActions';
import type {SessionActions} from './SessionActions';
import type {SignalerenActions} from './SignalerenActions';
import type {UpdateActions} from './UpdateAction';
import type {VisueleInspectieActions} from './VisueleInspectieActions';
import type {ZorginstellingActions} from './ZorginstellingActions';
import type {ConnectionActions} from './ConnectionActions';
import type {ClearCacheActions} from './ClearCacheActions';
import type {KwaliteitsopnameOrmAction} from './KwaliteitsopnameOrmActions';
import type {DubbeleInstantieActions} from './DubbeleInstantieActions';
import type {LoginStatusActions} from './LoginStatusActions';
import type {OpgehaaldeDagenActions} from './OpgehaaldeDagenActions';
import type {MammografenStatusActions} from './MammografenStatusActions';
import type {ConnectieStatusActions} from './ConnectieStatusActions';

export type SeAction =
    AanvullendeInformatieActions
    | AfspraakActions
    | AutorisatieActions
    | ClientActions
    | DaglijstDatumActions
    | DagverslagActions
    | EnvironmentInfoActions
    | ErrorActions
    | FormActions
    | HuisartsActions
    | LogGebeurtenisActions
    | MammograafActions
    | NavigationActions
    | MBBSignaleringActions
    | OnderzoekActions
    | PassantAfspraakMakenActions
    | PlanningActions
    | PopupActions
    | SeGebruikersActions
    | SessionActions
    | SignalerenActions
    | UpdateActions
    | VisueleInspectieActions
    | WijzigingenActions
    | ZorginstellingActions
    | ConnectionActions
    | ClearCacheActions
    | KwaliteitsopnameOrmAction
    | DubbeleInstantieActions
    | LoginStatusActions
    | OpgehaaldeDagenActions
    | MammografenStatusActions
    | ConnectieStatusActions
    ;
