import {connect} from "react-redux"
import DagStatistiekenView, {DagStatistiekenViewProps} from "./DagStatistiekenView"
import {RootState} from "../../Store"
import {getIfExists} from "../../util/MapUtil"
import {getAantalNietBeeindigdeAfsprakenMetStatus, getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus, getTotaalAfsprakenDag} from "../../selectors/AfspraakSelectors"

const mapStateToProps = (state: RootState): DagStatistiekenViewProps => {
	return {
		dagverslag: getIfExists(state.dagverslag, state.daglijstDatum),
		aantalVerwacht: getAantalNietBeeindigdeAfsprakenMetStatus(state, "VERWACHT"),
		aantalIngeschreven: getAantalNietBeeindigdeAfsprakenMetStatus(state, "INGESCHREVEN"),
		aantalOnderzoek: getAantalNietBeeindigdeAfsprakenMetStatus(state, "ONDERZOEK"),
		aantalSignaleren: getAantalNietBeeindigdeAfsprakenMetStatus(state, "SIGNALEREN"),
		aantalAfgerond: getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus(state, "AFGEROND"),
		aantalOnderbroken: getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus(state, "ONDERBROKEN"),
		aantalOnvolledig: getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus(state, "ONVOLLEDIG"),
		aantalTotaal: getTotaalAfsprakenDag(state),
	}
}

const DagStatistiekenContainer = connect(mapStateToProps)(DagStatistiekenView)
export default DagStatistiekenContainer