import React, {Component} from "react"
import AfspraakLijstView from "./AfspraakLijstView"
import type {Tijdslot} from "../../datatypes/Planning"
import {Afspraak} from "../../datatypes/Afspraak"
import {Col, Row} from "reactstrap"
import {store} from "../../Store"
import BarcodeReader from "react-barcode-reader"
import {GEFORCEERD_DAGLIJST_OPHALEN, vernieuwAfsprakenDaglijst} from "../../restclient/DaglijstRestclient"
import {vandaagISO} from "../../util/DateUtil"
import {Client} from "../../datatypes/Client"
import {navigateToClientgegevens, navigateToOnderzoek} from "../../util/NavigationUtil"

export type AfspraakOverzichtViewProps = {
	nietAfgerondeTijdSlots: Array<Tijdslot>;
	afgerondeTijdSlots: Array<Tijdslot>;
	clienten: Map<number, Client>;
	daglijstDatum: string;
};

export default class AfspraakOverzichtView extends Component<AfspraakOverzichtViewProps> {

	constructor(props: AfspraakOverzichtViewProps) {
		super(props)

		const state = store.getState()
		if (state.pendingUpdates && state.pendingUpdates.moetDaglijstNogVerversen) {
			vernieuwAfsprakenDaglijst()
		}
	}

	handleScan(gescandeData: string): void {
		gescandeData = gescandeData.replace("http:
		const afspraken = store.getState().afsprakenById
		afspraken.forEach((afspraak: Afspraak) => {
			if (String(afspraak.uitnodigingsNr) === gescandeData) {
				if (afspraak.vanafDatum !== vandaagISO()) {
					return
				}

				navigeerNaarClientAfspraak(afspraak)
				return
			}
		})
	}

	handleError(): void {
	}

	render(): JSX.Element {
		return <div className="afspraaklijst">
			<BarcodeReader onError={this.handleError} onScan={this.handleScan} minLength={9}/>
			<Row>
				<Col md={6}><h6>Gepland
					({this.props.nietAfgerondeTijdSlots.filter(value => value instanceof Afspraak).length})</h6></Col>
				{this.props.daglijstDatum === vandaagISO() &&
				<Col md={6}><i className="fa fa-refresh float-right" id={"daglijst-refresh"} aria-hidden="true"
							   onClick={(): void => {
								   vernieuwAfsprakenDaglijst(GEFORCEERD_DAGLIJST_OPHALEN)
							   }}/></Col>}
			</Row>
			<AfspraakLijstView afspraken={this.props.nietAfgerondeTijdSlots} clienten={this.props.clienten}
							   emptyText="Er zijn geen geplande afspraken."/>
			<h6>Afgerond ({this.props.afgerondeTijdSlots.filter(value => value instanceof Afspraak).length})</h6>
			<AfspraakLijstView afspraken={this.props.afgerondeTijdSlots} clienten={this.props.clienten}
							   emptyText="Er zijn geen afgeronde afspraken."/>
		</div>
	}

}

export const navigeerNaarClientAfspraak = (afspraak: Afspraak): void => {
	if (!store.getState().autorisatie.onderzoeken) {
		navigateToClientgegevens(store.dispatch, afspraak.clientId, afspraak.id)
	} else {
		switch (afspraak.status) {
			case "INGESCHREVEN":
				navigateToOnderzoek(store.dispatch, afspraak.clientId, afspraak.id, "Vorige onderzoeken")
				break
			case "ONDERZOEK":
				navigateToOnderzoek(store.dispatch, afspraak.clientId, afspraak.id, "Visuele inspectie")
				break
			case "SIGNALEREN":
				navigateToOnderzoek(store.dispatch, afspraak.clientId, afspraak.id, "Signaleren")
				break
			default:
				navigateToClientgegevens(store.dispatch, afspraak.clientId, afspraak.id)
		}
	}
}
