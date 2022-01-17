import {connect} from "react-redux"
import type {Afspraak} from "../../datatypes/Afspraak"
import AfspraakRijView, {AfspraakRijViewDispatchProps, AfspraakRijViewStateProps} from "./AfspraakRijView"
import {vandaagISO} from "../../util/DateUtil"
import {getIfExists} from "../../util/MapUtil"
import {navigeerNaarClientAfspraak} from "./AfspraakOverzichtView"
import {RootState} from "../../Store"
import {Client} from "../../datatypes/Client"

export type AfspraakRijContainerProps = {
	afspraak: Afspraak;
	client: Client;
}

const mapStateToProps = (state: RootState, ownProps: AfspraakRijContainerProps): AfspraakRijViewStateProps => {
	const onderzoek = getIfExists(state.onderzoekByAfspraakId, ownProps.afspraak.id)
	return {
		...ownProps,
		onderzoekStatus: onderzoek?.status,
		klikbaar: ownProps.afspraak ? ownProps.afspraak.vanafDatum === vandaagISO() || ownProps.afspraak.status !== "VERWACHT" : false,
	}
}

const mapDispatchToProps = (): AfspraakRijViewDispatchProps => ({
	onRijKlik(afspraak: Afspraak): void {
		navigeerNaarClientAfspraak(afspraak)
	},
})

const AfspraakRijContainer = connect(mapStateToProps, mapDispatchToProps)(AfspraakRijView)
export default AfspraakRijContainer