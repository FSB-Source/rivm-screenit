import {connect} from "react-redux"
import type {KwaliteitsopnameReden, VoorOfNaKalibratie} from "./KwaliteitsopnameView"
import KwaliteitsopnameView, {KwaliteitsopnameViewProps, KwaliteitsopnameViewStateProps} from "./KwaliteitsopnameView"
import {putTransactionToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {BEEINDIG_KWALITEITSOPNAME, createActionKwaliteitsopnameOrm, START_KWALITEITSOPNAME} from "../../actions/KwaliteitsopnameOrmActions"
import {RootState, store} from "../../Store"
import {beeindigKwaliteitsopname, kwaliteitsopnameToevoegenAanWerklijst} from "../../restclient/WerklijstRestclient"
import {aeTitle, seCode} from "../../util/Util"
import {fetchApiPromise} from "../../util/ApiUtil"
import {createBeeindigBezigMetKwaliteitsopnameAction, createStartBezigMetKwaliteitsopnameAction} from "../../actions/BezigMetKwaliteitsopnameActions"
import {nu, nuISO} from "../../util/DateUtil"
import {Dispatch} from "redux"
import {navigateToDaglijst} from "../../util/NavigationUtil"

const mapDispatchToProps = (dispatch: Dispatch): KwaliteitsopnameViewProps => ({
	onKwaliteitsopnameAction(actionType: string, reden: KwaliteitsopnameReden, voorOfNaKalibratie: VoorOfNaKalibratie): void {
		const mammograafnr = aeTitle().substr(aeTitle().length - 1, 1)

		if (actionType === START_KWALITEITSOPNAME) {
			fetchApiPromise("GET", `kwaliteitsopname/${mammograafnr}`).then(response => {
				response.json().then((volgnr: number) => {
					startOfBeeindigKwaliteitsopname(mammograafnr, volgnr, actionType, reden, voorOfNaKalibratie, dispatch)
				})
			})
		} else {
			const bezigMetKwaliteitsopnameVolgnr = store.getState().bezigMetKwaliteitsopnameVolgnr
			startOfBeeindigKwaliteitsopname(mammograafnr, bezigMetKwaliteitsopnameVolgnr !== null ? bezigMetKwaliteitsopnameVolgnr : undefined, actionType, reden, voorOfNaKalibratie, dispatch)
		}
	},
})

const mapStateToProps = (state: RootState): KwaliteitsopnameViewStateProps => {
	return {tomosyntheseMogelijk: true === state.environmentInfo?.tomosyntheseMogelijk}
}

const startOfBeeindigKwaliteitsopname = (mammograafnr: string, volgnr: number | undefined, actionType: string, reden: KwaliteitsopnameReden, voorOfNaKalibratie: VoorOfNaKalibratie, dispatch: any): void => {
	if (!volgnr) {
		console.warn(`Volgnummer is leeg bij starten of beeindigen kwaliteitsopname op mammograaf ${mammograafnr}`)
		return
	}
	const session = store.getState().session
	if (!session) {
		console.warn(`Sessie is leeg bij starten of beeindigen kwaliteitsopname op mammograaf ${mammograafnr}`)
		return
	}
	const medewerkercode = session.medewerkercode
	const datumAcccesionNumber = nu().format("DDMMYY")

	const datumPatientId = nu().format("YYYYMMDD")
	const startMoment = nuISO()
	const kalibratieLetter = voorOfNaKalibratie === "Na kalibratie" ? "N" : voorOfNaKalibratie === "Voor kalibratie" ? "V" : "G"
	const seNr = seCode().split("-")[1]
	const volgnrString = (volgnr < 10 ? "0" : "") + volgnr
	const accessionNumber = `${bepaalRedenCodeAccesionNumber(reden) + seNr}M${mammograafnr}${datumAcccesionNumber}${kalibratieLetter}${volgnrString}`
	const onderzoekscode = bepaalOnderzoeksCode(reden)
	const patientId = `${seCode()}_LRCB_${datumPatientId}`

	switch (actionType) {
		case START_KWALITEITSOPNAME:
			dispatch(createStartBezigMetKwaliteitsopnameAction(volgnr))
			kwaliteitsopnameToevoegenAanWerklijst({
				aeTitle: aeTitle(),
				medewerkercode,
				reden,
				voorOfNaKalibratie,
				seCode: seCode(),
				accessionNumber,
				onderzoekscode,
				startMoment,
				patientId,
			})
			break
		case BEEINDIG_KWALITEITSOPNAME:
			dispatch(createBeeindigBezigMetKwaliteitsopnameAction(volgnr))
			beeindigKwaliteitsopname(aeTitle())
			break
	}

	const actionOrm = createActionKwaliteitsopnameOrm(actionType, seCode(), reden, patientId, accessionNumber, onderzoekscode)
	dispatch(actionOrm)
	putTransactionToScreenItCentraalPromise(undefined, actionType === START_KWALITEITSOPNAME ? "START_KWALITEITSOPNAME_TRANSACTION" : "BEEINDIG_KWALITEITSOPNAME_TRANSACTION", actionOrm)

	if (actionType === BEEINDIG_KWALITEITSOPNAME) {
		navigateToDaglijst(dispatch)
	}
}
const bepaalRedenCodeAccesionNumber = (reden: KwaliteitsopnameReden): string => {
	if ("Vervanging rontgenbuis" === reden) {
		return "BU"
	}
	if ("Dagelijkse tomosynthese" === reden) {
		return "TO"
	}
	return "QC"
}

const bepaalOnderzoeksCode = (reden: KwaliteitsopnameReden): string => {
	if ("Vervanging rontgenbuis" === reden) {
		return "LRCBBU"
	}
	if ("Dagelijkse tomosynthese" === reden) {
		return "LRCBDBT"
	}
	return "LRCBQC"
}

const KwaliteitsopnameContainer = connect(mapStateToProps, mapDispatchToProps)(KwaliteitsopnameView)
export default KwaliteitsopnameContainer
