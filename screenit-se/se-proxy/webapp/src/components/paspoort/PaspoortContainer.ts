import {connect} from "react-redux"
import type {PaspoortProps} from "./PaspoortView"
import PaspoortView from "./PaspoortView"
import {ligtTussenData, vandaagISO} from "../../util/DateUtil"
import {RootState} from "../../Store"
import {Client} from "../../datatypes/Client"
import {Afspraak} from "../../datatypes/Afspraak"

export type PaspoortContainerProps = {
	client: Client;
	afspraak: Afspraak;
}

const mapStateToProps = (state: RootState, ownProps: PaspoortContainerProps): PaspoortProps => {
	const tijdelijkAdres = ownProps.client.tijdelijkAdres

	if (tijdelijkAdres && ligtTussenData(vandaagISO(), tijdelijkAdres.startDatum, tijdelijkAdres.eindDatum)) {
		return {
			...ownProps,
			tijdelijkAdresValue: "Er is een tijdelijk adres beschikbaar",
		}
	} else {
		return {
			...ownProps,
			tijdelijkAdresValue: "Nee",
		}
	}
}

const PaspoortContainer = connect(mapStateToProps)(PaspoortView)
export default PaspoortContainer