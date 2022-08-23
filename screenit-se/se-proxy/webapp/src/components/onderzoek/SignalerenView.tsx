import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import PaspoortContainer from "../paspoort/PaspoortContainer"
import AutorisatieButton from "../generic/AutorisatieButton"
import type {Onderzoek} from "../../datatypes/Onderzoek"
import {Col, Row} from "reactstrap"
import AfwijkingenContainer from "./signaleren/AfwijkingenContainer"
import type {Signalering} from "../../datatypes/Signalering"
import MbbSignaleringContainer from "./inspectie/MbbSignaleringContainer"
import AanvullendeInformatieContainer from "./inspectie/AanvullendeInformatieContainer"
import type {Form} from "../../datatypes/Form"

export type SignalerenViewStateProps = {
	afspraak: Afspraak;
	onderzoek: Onderzoek;
	client: Client;
	magSignaleren: boolean;
	heeftWijzigingen: boolean;
	isEditable: boolean;
	signalering: Signalering;
	aanvullendeInformatieForm: Form;
	online: boolean;
};

export type SignalerenViewDispatchProps = {
	onVorige: (client: Client, afspraak: Afspraak) => void;
	onVolgende: (afspraak: Afspraak, client: Client, onderzoek: Onderzoek, signalering: Signalering, form: Form, alleenOpslaan?: boolean) => void;
	onInitializeForm: (client: Client) => void;
};

export default class SignalerenView extends Component<SignalerenViewStateProps & SignalerenViewDispatchProps> {

	constructor(props: SignalerenViewStateProps & SignalerenViewDispatchProps) {
		super(props)
		this.props.onInitializeForm(this.props.client)
	}

	render(): JSX.Element {
		const client: Client = this.props.client
		const afspraak: Afspraak = this.props.afspraak
		return <div className="tabpagina">
			<div className="row">
				<div className="onderzoek-heading">
					<h1 className="float-left">Signaleren</h1>
					{this.props.afspraak.status !== "BEEINDIGD" &&
						<AutorisatieButton
							id="afrondenButton" label={"Afronden"} heeftRecht={this.props.magSignaleren}
							online={this.props.online} rechtNaam={"Signaleren op SE"}
							onClick={(): void => {
								this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.signalering, this.props.aanvullendeInformatieForm)
							}}/>
					}
					{this.props.heeftWijzigingen &&
						<AutorisatieButton
							id="opslaanButton" label={"Opslaan"} rechtNaam={"Signaleren op SE"}
							heeftRecht={this.props.magSignaleren}
							className="float-right btn btn-primary-se"
							onClick={(): void => {
								this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.signalering, this.props.aanvullendeInformatieForm, true)
							}}/>
					}
					<button className="float-right btn btn-primary-se" onClick={(): void => {
						this.props.onVorige(client, afspraak)
					}}>Vorige
					</button>
				</div>
			</div>
			<Row>
				<PaspoortContainer client={client} afspraak={afspraak}/>
			</Row>
			<Row className="do-not-select">
				<AfwijkingenContainer afspraakId={this.props.afspraak.id} signalering={this.props.signalering}
									  isEditable={this.props.isEditable && this.props.magSignaleren}
									  amputatie={this.props.onderzoek.amputatie}/>
			</Row>
			<Row className="onderzoek-row do-not-select">
				<Col md={2}/>
				<Col md={4}>
					<MbbSignaleringContainer disabled={!this.props.magSignaleren}/>
				</Col>
				<Col md={4}>
					<AanvullendeInformatieContainer disabled={!this.props.magSignaleren}/>
				</Col>
			</Row>
		</div>
	}

}
