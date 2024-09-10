import {combineReducers} from "redux"
import AfsprakenReducer from "./AfspraakReducer"
import DaglijstDatumReducer from "./DaglijstDatumReducer"
import DagverslagReducer from "./DagverslagReducer"
import NietAfgeslotenVanafReducer from "./NietAfgeslotenVanafReducer"
import SeGebruikersReducer from "./SeGebruikersReducer"
import SessionReducer from "./SessionReducer"
import ClientReducer from "./ClientReducer"
import NavigationReducer from "./NavigationReducer"
import PlanningReducer from "./PlanningReducer"
import VisueleInspectieReducer from "./VisueleInspectieReducer"
import EnvironmentInfoReducer from "./EnvironmentInfoReducer"
import AutorisatieReducer from "./AutorisatieReducer"
import FormReducer from "./FormReducer"
import HuisartsenReducer from "./HuisartsenReducer"
import UpdateReducer from "./UpdateReducer"
import WijzigingenReducer from "./WijzigingenReducer"
import BezigMetKwaliteitsopnameVolgnrReducer from "./BezigMetKwaliteitsopnameVolgnrReducer"
import ImsReducer from "./ImsReducer"
import SignalerenReducer from "./SignalerenReducer"
import ZorginstellingenReducer from "./ZorginstellingenReducer"
import OnderzoekReducer from "./OnderzoekReducer"
import ErrorReducer from "./ErrorReducer"
import MammografenReducer from "./MammografenReducer"
import PopupReducer from "./PopupReducer"
import ConnectionReducer from "./ConnectionReducer"
import MammograafReducer from "./MammograafReducer"
import DubbeleInstantieReducer from "./DubbeleInstantieReducer"
import OpgehaaldeDagenReducer from "./OpgehaaldeDagenReducer"
import ConnectieStatusReducer from "./ConnectieStatusReducer"
import MammografenStatusReducer from "./MammografenStatusReducer"
import LoginStatusReducer from "./LoginStatusReducer"
import WebsocketStatusReducer from "./WebsocketStatusReducer"

const seReducers = combineReducers({
	afsprakenById: AfsprakenReducer,
	daglijstDatum: DaglijstDatumReducer,
	dagverslag: DagverslagReducer,
	nietAfgeslotenVanaf: NietAfgeslotenVanafReducer,
	seGebruikers: SeGebruikersReducer,
	clientenById: ClientReducer,
	planning: PlanningReducer,
	session: SessionReducer,
	navigation: NavigationReducer,
	visueleInspectieAfbeeldingByAfspraakId: VisueleInspectieReducer,
	formsByFormId: FormReducer,
	environmentInfo: EnvironmentInfoReducer,
	autorisatie: AutorisatieReducer,
	onderzoekByAfspraakId: OnderzoekReducer,
	signaleringByAfspraakId: SignalerenReducer,
	huisartsenById: HuisartsenReducer,
	zorginstellingen: ZorginstellingenReducer,
	error: ErrorReducer,
	pendingUpdates: UpdateReducer,
	mammografenById: MammografenReducer,
	huidigeMammograafId: MammograafReducer,
	popup: PopupReducer,
	heeftWijzigingen: WijzigingenReducer,
	online: ConnectionReducer,
	activeStudyForIms: ImsReducer,
	bezigMetKwaliteitsopnameVolgnr: BezigMetKwaliteitsopnameVolgnrReducer,
	dubbeleInstantie: DubbeleInstantieReducer,
	loginStatus: LoginStatusReducer,
	opgehaaldeDagen: OpgehaaldeDagenReducer,
	connectieStatus: ConnectieStatusReducer,
	mammografenStatus: MammografenStatusReducer,
	websocketStatus: WebsocketStatusReducer,
})

export default seReducers
