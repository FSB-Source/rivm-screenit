import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {maakWerklijstItem} from "./OnderzoekContainer"
import type {ClientWerklijstItem} from "../../datatypes/ClientWerklijstItem"
import VisueleInspectieAfbeeldingContainer from "./inspectie/VisueleInspectieAfbeeldingContainer"
import PaspoortContainer from "../paspoort/PaspoortContainer"
import type {AnnotatieAfbeelding} from "../../datatypes/AnnotatieAfbeelding"
import {Col, Row} from "reactstrap"
import MbbSignaleringContainer from "./inspectie/MbbSignaleringContainer"
import type {Onderzoek} from "../../datatypes/Onderzoek"
import AanvullendeInformatieContainer from "./inspectie/AanvullendeInformatieContainer"
import type {Form} from "../../datatypes/Form"
import {meldingAfgeslotenProcedure} from "../../restclient/WerklijstRestclient"
import {OnderzoekType} from "../../datatypes/OnderzoekType"

export type VisueleInspectieViewStateProps = {
	afspraak: Afspraak;
	client: Client;
	onderzoek: Onderzoek;
	afbeelding: AnnotatieAfbeelding;
	magSignaleren: boolean;
	aanvullendeInformatieForm: Form;
	heeftWijzigingen: boolean;
	aeTitle?: string;
	isEditable: boolean;
	medewerkercode?: string;
};

export type VisueleInspectieViewDispatchProps = {
	onVorige: (afspraak: Afspraak) => void;
	onVolgende: (afspraak: Afspraak, client: Client, onderzoek: Onderzoek, afbeelding: AnnotatieAfbeelding, magSignaleren: boolean, form: Form, alleenOpslaan: boolean) => void;
	toevoegenAanWerklijst: (werklijstItem: ClientWerklijstItem) => void;
	verwijderVanWerklijst: (aeTitle: string) => void;
	onInitializeForm: (client: Client) => void;
}

type VisueleInspectieState = {
	width: number;
};

export default class VisueleInspectieView extends Component<VisueleInspectieViewStateProps & VisueleInspectieViewDispatchProps, VisueleInspectieState> {

	constructor(props: VisueleInspectieViewStateProps & VisueleInspectieViewDispatchProps) {
		super(props)
		this.state = {
			width: 0,
		}
		this.props.onInitializeForm(this.props.client)
	}

	componentWillUnmount(): void {
		if (this.props.aeTitle) {
			this.props.verwijderVanWerklijst(this.props.aeTitle)
		}
	}

	componentDidMount(): void {
		const element = document.getElementById("col-afbeelding-container")
		if (element) {
			this.setState({
				width: element.clientWidth - parseFloat(window.getComputedStyle(element).paddingRight),
			})
		}

		this.voegOnderzoekTypeWijzigingToeAanWerklijst(this.props.onderzoek.onderzoekType)
	}

	voegOnderzoekTypeWijzigingToeAanWerklijst(onderzoekType: OnderzoekType): void {
		if (this.props.aeTitle && !this.props.afspraak.doorgevoerd && this.props.medewerkercode) {
			this.props.toevoegenAanWerklijst(maakWerklijstItem(this.props.afspraak, this.props.client, this.props.aeTitle, this.props.medewerkercode, onderzoekType))
		}
	}

	render(): JSX.Element {
		const client = this.props.client
		const afspraak = this.props.afspraak
		return <div>
			<Row>
				<div className="onderzoek-heading page-heading-fixed">
					<h1 className="float-left">Visuele inspectie</h1>
					<button className="float-right btn btn-primary-se ml-3" onClick={(): void => {
						meldingAfgeslotenProcedure(client.id, this.props.afspraak.uitnodigingsNr)
						this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.afbeelding, this.props.magSignaleren, this.props.aanvullendeInformatieForm, false)
					}}>
						{this.props.afspraak.status === "ONDERZOEK" ? "Signaleren" : "Volgende"}
					</button>
					{this.props.heeftWijzigingen && <button className="float-right btn btn-primary-se ml-3" onClick={(): void => {
						this.props.onVolgende(afspraak, this.props.client, this.props.onderzoek, this.props.afbeelding, this.props.magSignaleren, this.props.aanvullendeInformatieForm, true)
					}}>
						Opslaan
					</button>}
					<button className="float-right btn btn-primary-se" onClick={(): void => {
						this.props.onVorige(afspraak)
					}}>Vorige
					</button>
				</div>
			</Row>
			<div className="onderzoek-content">
				<div className="tabpagina">
					<Row>
						<PaspoortContainer client={client} afspraak={afspraak}/>
					</Row>
					<Row className="onderzoek-row do-not-select">
						<Col id={"col-afbeelding-container"} md={5}>
							<VisueleInspectieAfbeeldingContainer width={this.state.width} isEditable={this.props.isEditable}/>
						</Col>
						<Col md={3}>
							<MbbSignaleringContainer disabled={false}/>
						</Col>
						<Col md={4} className={"pr-0"}>
							<AanvullendeInformatieContainer
								disabled={false}
								voegOnderzoekTypeWijzigingToeAanWerklijst={(onderzoekType: OnderzoekType): void => {
									this.voegOnderzoekTypeWijzigingToeAanWerklijst(onderzoekType)
								}}
							/>
						</Col>
					</Row>
				</div>
			</div>
		</div>
	}

}
