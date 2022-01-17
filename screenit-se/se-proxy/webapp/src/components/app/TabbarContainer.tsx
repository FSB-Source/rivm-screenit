import {connect} from "react-redux"
import {leesAfspraken} from "../../restclient/DaglijstRestclient"
import TabbarView, {TabbarViewDispatchProps, TabbarViewStateProps} from "./TabbarView"
import type {Tab} from "../../datatypes/Navigation"
import {getIfExists} from "../../util/MapUtil"
import {showErrorToast} from "../../util/ToastUtil"
import {RootState, store} from "../../Store"
import {getDagverslag} from "../../restclient/DagverslagRestClient"
import {datumInVerleden, isAfter, vandaagISO, vandaagPlusDagen} from "../../util/DateUtil"
import {hasNietDoorgevoerdeOnderzoeken, hasOpenstaandeOnderzoeken} from "../../util/AfsprakenUtil"
import {createActionWijzigingenVerwerkt} from "../../actions/WijzigingenActions"
import {showWijzigingenPopup} from "../../util/PopupUtil"
import {createActionDeleteVisueleInspectieAfbeeldingByAfspraakId} from "../../actions/VisueleInspectieActions"
import {Dispatch} from "redux"
import {createActionKiesDaglijstDatum} from "../../actions/DaglijstDatumActions"
import {
	navigateToClientgegevens,
	navigateToConnectiestatus,
	navigateToDaglijst,
	navigateToDagverslag,
	navigateToKwaliteitsopname,
	navigateToOnderzoek,
} from "../../util/NavigationUtil"

const mapStateToProps = (state: RootState): TabbarViewStateProps => {
	const afspraak = getIfExists(state.afsprakenById, state.navigation.afspraakId)
	const activeTab: Tab = state.navigation.tab
	const daglijstTabClickable = ((!datumInVerleden(state.daglijstDatum) &&
				(state.environmentInfo !== null && !isAfter(state.daglijstDatum, vandaagPlusDagen(state.environmentInfo.dagenDaglijstOphalenLimiet))))
			|| hasOpenstaandeOnderzoeken(state.daglijstDatum)
			|| hasNietDoorgevoerdeOnderzoeken(state.daglijstDatum))
		&& state.bezigMetKwaliteitsopnameVolgnr === null && state.autorisatie.inschrijven
	return {
		activeTab: activeTab,
		afspraak: afspraak,
		daglijstTabClickable: daglijstTabClickable,
		clientgegevensTabClickable: activeTab === "Onderzoek",
		onderzoekTabClickable: activeTab === "Cliëntgegevens" && afspraak !== undefined && afspraak.status !== "VERWACHT" && state.autorisatie.onderzoeken,
		dagverslagTabClickable: activeTab === "Daglijst" && state.online,
		kwaliteitsopnameTabClickable: state.autorisatie.kwaliteitsopname && !datumInVerleden(state.daglijstDatum) && !!state.huidigeMammograafId && state.online,
		connectiestatusTabClickable: state.autorisatie.connectiestatus,
		subPagina: state.navigation.subPagina,
		magOnderzoeken: state.autorisatie.onderzoeken,
		error: state.error || undefined,
		klaarMetDataLaden: state.opgehaaldeDagen.has(state.daglijstDatum),
		daglijstDatum: state.daglijstDatum,
		huidigeMammograafId: state.huidigeMammograafId !== null ? state.huidigeMammograafId : undefined,
		online: state.online,
		bezigMetKwaliteitsopnameVolgnr: state.bezigMetKwaliteitsopnameVolgnr !== null ? state.bezigMetKwaliteitsopnameVolgnr : undefined,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): TabbarViewDispatchProps => ({
	onClickTab(tab: Tab, props: TabbarViewStateProps): void {
		if (store.getState().heeftWijzigingen) {
			showWijzigingenPopup(() => {
				leesAfspraken(store.getState().daglijstDatum, undefined, false)

				if (props.afspraak && props.subPagina && props.subPagina === "Visuele inspectie") {
					dispatch(createActionDeleteVisueleInspectieAfbeeldingByAfspraakId(props.afspraak.id))
				}

				dispatch(createActionWijzigingenVerwerkt())
				navigeerNaarTab(dispatch, tab, props)
			}, dispatch)
		} else {
			navigeerNaarTab(dispatch, tab, props)
		}
	},

})

const navigeerNaarTab = (dispatch: Dispatch, tab: Tab, props: TabbarViewStateProps): void => {
	const afspraak = props.afspraak
	switch (tab) {
		case "Daglijst":
			if (props.daglijstTabClickable) {
				navigateToDaglijst(dispatch)
			} else if (props.bezigMetKwaliteitsopnameVolgnr !== undefined) {
				showErrorToast("Beëindig de kwaliteitsopname voordat u naar de daglijst navigeert")
			}
			break
		case "Cliëntgegevens":
		case "Cli\xEBntgegevens":
			if (afspraak) {
				navigateToClientgegevens(dispatch, afspraak.clientId, afspraak.id)
			} else {
				showErrorToast("Klik in de daglijst op een afspraak om de cliëntgegevens te zien.")
			}
			break
		case "Onderzoek":
			if (props.magOnderzoeken) {
				if (afspraak) {
					switch (afspraak.status) {
						case "VERWACHT":
							showErrorToast("Schrijf de client eerst in.")
							break
						case "INGESCHREVEN":
							navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Vorige onderzoeken")
							break
						case "ONDERZOEK":
							navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Visuele inspectie")
							break
						case "SIGNALEREN":
							navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Signaleren")
							break
						case "BEEINDIGD":
							navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Signaleren")
							break
						case "KWALITEITSOPNAME":
							navigateToKwaliteitsopname(dispatch)
							break
						default:
							break
					}
				} else {
					showErrorToast("Klik in de daglijst op een ingeschreven client om het onderzoek te doen, of schrijf een client in.")
				}
			} else {
				showErrorToast("Hiervoor heeft u niet de benodigde autorisatie: Onderzoek starten op SE.")
			}
			break
		case "Dagverslag":
			if (props.dagverslagTabClickable) {
				getDagverslag(store.getState().daglijstDatum).then(() => {
					navigateToDagverslag(dispatch)
				})
			} else if (!props.online) {
				showErrorToast("Het dagverslag is niet inzichtelijk wanneer de SE offline is.")
			} else {
				showErrorToast("Het dagverslag is alleen toegankelijk vanuit de daglijst.")
			}
			break
		case "Kwaliteitsopname":
			if (props.kwaliteitsopnameTabClickable) {
				dispatch(createActionKiesDaglijstDatum(vandaagISO()))
				navigateToKwaliteitsopname(dispatch)
			} else if (!props.huidigeMammograafId) {
				showErrorToast("Kwaliteitsopname is niet mogelijk omdat er aan dit werkstation geen mammograaf gekoppeld is.")
			} else if (datumInVerleden(props.daglijstDatum)) {
				showErrorToast("Kwaliteitsopname is niet mogelijk omdat de datum van daglijst en dagverslag in het verleden ligt.")
			} else if (!props.online) {
				showErrorToast("Kwaliteitsopname is niet mogelijk wanneer de SE offline is.")
			} else {
				showErrorToast("U heeft geen autorisatie voor de kwaliteitsopname.")
			}
			break
		case "Connectiestatus":
			if (props.connectiestatusTabClickable) {
				dispatch(createActionKiesDaglijstDatum(vandaagISO()))
				navigateToConnectiestatus(dispatch)
			} else {
				showErrorToast("U heeft geen autorisatie om de connectiestatus in te zien.")
			}
			break
		default:
			break
	}
}

const TabbarContainer = connect(mapStateToProps, mapDispatchToProps)(TabbarView)
export default TabbarContainer
