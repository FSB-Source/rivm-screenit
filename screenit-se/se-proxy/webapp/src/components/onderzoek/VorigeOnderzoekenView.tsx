import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {vorigeOnderzoekenVervolgBtnKind} from "./VorigeOnderzoekenContainer"
import PaspoortContainer from "../paspoort/PaspoortContainer"
import AutorisatieButton from "../generic/AutorisatieButton"
import VorigOnderzoekContainer from "./vorigeOnderzoeken/VorigOnderzoekContainer"
import type {VorigOnderzoek} from "../../datatypes/VorigOnderzoek"
import Paneel from "../generic/Paneel"
import {Row} from "reactstrap"
import {MAMMOGRAFIE, OnderzoekType, TOMOSYNTHESE} from "../../datatypes/OnderzoekType"

export type VorigeOnderzoekenConfirmBtnKind = "Onderzoek starten" | "Mammografie starten" | "Volgende";

export type VorigeOnderzoekenViewStateProps = {
	afspraak: Afspraak;
	client: Client;
	magOnderzoeken: boolean;
	gebruikersnaam?: string;
	setHeeftOudeBeeldenOpgevraagd: () => void;
	tomosyntheseMogelijk: boolean;
	kanTomosyntheseStarten: boolean;
};

export type VorigeOnderzoekenViewDispatchProps = {
	onVervolgButton: (client: Client, afspraak: Afspraak, onderzoekType: OnderzoekType, confirmBtnKind: VorigeOnderzoekenConfirmBtnKind) => void;
};

export default class VorigeOnderzoekenView extends Component<VorigeOnderzoekenViewStateProps & VorigeOnderzoekenViewDispatchProps> {

	render(): JSX.Element {
		const client = this.props.client
		const afspraak = this.props.afspraak
		const alleVorigeOnderzoekenAanwezig = Math.min(afspraak.aantalOpgekomen, 3) === this.props.client.vorigeOnderzoeken.length
		const confirmBtnKind: VorigeOnderzoekenConfirmBtnKind = vorigeOnderzoekenVervolgBtnKind(this.props.afspraak, this.props.tomosyntheseMogelijk)
		return <div>
			<Row>
				<div className="onderzoek-heading page-heading-fixed">
					<h1 className="float-left">Vorige onderzoeken</h1>
					<AutorisatieButton id="vorigeOnderzoekenVervolgButton"
									   label={confirmBtnKind}
									   heeftRecht={this.props.magOnderzoeken} rechtNaam={"Onderzoek starten op SE."}
									   className={"float-right btn btn-primary-se ml-3"}
									   onClick={(): void => {
										   this.props.onVervolgButton(client, afspraak, MAMMOGRAFIE, confirmBtnKind)
									   }}/>
					{this.props.kanTomosyntheseStarten &&
						<AutorisatieButton id="tomosyntheseButton"
										   label="Tomosynthese starten"
										   heeftRecht={this.props.magOnderzoeken} rechtNaam={"Onderzoek starten op SE."}
										   onClick={(): void => {
											   this.props.onVervolgButton(client, afspraak, TOMOSYNTHESE, confirmBtnKind)
										   }}/>
					}
				</div>
			</Row>
			<div className="onderzoek-content">
				<div className="tabpagina">
					<Row>
						<PaspoortContainer client={client} afspraak={afspraak}/>
					</Row>
					{alleVorigeOnderzoekenAanwezig ? null : <Row key={"OnderzoekInformatieOntbreekt"}>
						<Paneel className={"onderzoek-component paneel-shadow"}>
							Door een technische fout kan de vorige onderzoeksinformatie (gedeeltelijk) niet weergegeven worden.
							U kunt het onderzoek normaal verder uitvoeren
						</Paneel>
					</Row>}
					{this.props.client.vorigeOnderzoeken.map((vorigOnderzoek: VorigOnderzoek, index: number) => {
						return <div className="row" key={index}>
							<VorigOnderzoekContainer vorigOnderzoek={vorigOnderzoek} meestRecent={index === 0}
													 client={this.props.client} gebruikersnaam={this.props.gebruikersnaam}
													 setHeeftOudeBeeldenOpgevraagd={this.props.setHeeftOudeBeeldenOpgevraagd}/>
						</div>
					})}
				</div>
			</div>
		</div>
	}
}