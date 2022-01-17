import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import IdentificatieContainer from "./IdentificatieContainer"
import type {Client} from "../../datatypes/Client"
import TelefoonContainer from "./TelefoonContainer"
import {Button, Row} from "reactstrap"
import PaspoortContainer from "../paspoort/PaspoortContainer"
import BezwaarContainer from "./BezwaarContainer"
import EmailContainer from "./EmailContainer"
import type {Form} from "../../datatypes/Form"
import HuisartsContainer from "./huisarts/HuisartsContainer"
import BarcodeReader from "react-barcode-reader"
import {parseMRZ} from "../../util/mrzParser"
import {createActionKiesIdentificatienummer, createActionKiesIdentificatiesoort} from "../../actions/AfspraakActions"
import {store} from "../../Store"
import {createActionWijzigingIdentificatie} from "../../actions/WijzigingenActions"
import AdresView from "./adres/AdresView"

export type ClientgegevensViewStateProps = {
	client: Client;
	afspraak: Afspraak;
	heeftwijzigingen: boolean;
	clientGegevensForm: Form;
};

export type ClientgegevensViewDispatchProps = {
	onConfirm: (clientGegevensForm: Form, afspraak: Afspraak, client: Client, alleenOpslaan: boolean) => void;
	onInitializeForm: (client: Client, afspraak: Afspraak) => void;
	onConfirmUitschrijven: (client: Client, afspraak: Afspraak) => void;
}

export default class ClientgegevensView extends Component<ClientgegevensViewStateProps & ClientgegevensViewDispatchProps> {
	constructor(props: ClientgegevensViewStateProps & ClientgegevensViewDispatchProps) {
		super(props)
		this.handleScan = this.handleScan.bind(this)
		this.props.onInitializeForm(this.props.client, this.props.afspraak)
	}

	onConfirmClientgegevens = (): void => {
		this.props.onConfirm(this.props.clientGegevensForm, this.props.afspraak, this.props.client, false)
	}

	onOpslaanClientgegevens = (): void => {
		this.props.onConfirm(this.props.clientGegevensForm, this.props.afspraak, this.props.client, true)
	}

	onConfirmUitschrijven = (): void => {
		this.props.onConfirmUitschrijven(this.props.client, this.props.afspraak)
	}

	handleScan: (...args: Array<any>) => any = (gescandeData: string) => {
		const afspraakId = this.props.afspraak.id
		const mrz = parseMRZ(gescandeData)

		if (mrz.id_type === "RIJBEWIJS") {
			store.dispatch(createActionKiesIdentificatiesoort(afspraakId, mrz.id_type))
			store.dispatch(createActionKiesIdentificatienummer(afspraakId, mrz.id_nummer))
			store.dispatch(createActionWijzigingIdentificatie())
		}
	}

	handleError: (...args: Array<unknown>) => void = () => {
	}

	render(): JSX.Element {
		const client: Client = this.props.client
		return <div className="inschrijven-scherm">
			<BarcodeReader onError={this.handleError} onScan={this.handleScan} minLength={30}/>
			<div className="tabpagina">
				<Row>
					<div className={"onderzoek-heading"}>
						<h1 className="float-left">Cliëntgegevens</h1>
						{this.props.afspraak.status === "VERWACHT" ?
							<Button
								className="btn-primary-se float-right"
								onClick={this.onConfirmClientgegevens}>Inschrijven</Button> :
							this.props.afspraak.status === "INGESCHREVEN" &&
							<Button
								className="float-right" color={"danger"}
								onClick={this.onConfirmUitschrijven}>Uitschrijven</Button>}
						{this.props.heeftwijzigingen ? <Button className="btn-primary-se float-right"
															   onClick={this.onOpslaanClientgegevens}>Opslaan</Button> : null}
					</div>
				</Row>
				<Row>
					<PaspoortContainer client={client} afspraak={this.props.afspraak}/>
				</Row>
				<Row>
					<IdentificatieContainer afspraak={this.props.afspraak} disabled={this.props.afspraak.doorgevoerd}/>
				</Row>
				<Row>
					<HuisartsContainer afspraak={this.props.afspraak} disabled={this.props.afspraak.doorgevoerd}/>
				</Row>
				<Row>
					<AdresView clientId={client.id} adres={client.adres}
							   tijdelijkGbaAdres={client.tijdelijkGbaAdres} tijdelijkAdres={client.tijdelijkAdres}
							   disabled={this.props.afspraak.doorgevoerd}/>
				</Row>
				<Row>
					<EmailContainer
						clientId={client.id} emailadres={client.emailadres}
						disabled={this.props.afspraak.doorgevoerd}/>
				</Row>
				<Row>
					<TelefoonContainer telefoonnummer1={client.telefoonnummer1} telefoonnummer2={client.telefoonnummer2}
									   disabled={this.props.afspraak.doorgevoerd}/>
				</Row>
				<Row>
					<BezwaarContainer afspraakId={this.props.afspraak.id}
									  bezwaarAangevraagd={this.props.afspraak.bezwaarAangevraagd}
									  bezwaarDoorgevoerdOpCentraal={this.props.afspraak.bezwaarDoorgevoerdOpCentraal}
									  isIngeschreven={this.props.afspraak.doorgevoerd}/>
				</Row>
			</div>
		</div>
	}

}