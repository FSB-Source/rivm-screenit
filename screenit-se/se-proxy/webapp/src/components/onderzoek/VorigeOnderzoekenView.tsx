import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {vorigeOnderzoekenConfirmBtnKind} from "./VorigeOnderzoekenContainer"
import PaspoortContainer from "../paspoort/PaspoortContainer"
import AutorisatieButton from "../generic/AutorisatieButton"
import VorigOnderzoekContainer from "./vorigeOnderzoeken/VorigOnderzoekContainer"
import type {VorigOnderzoek} from "../../datatypes/VorigOnderzoek"
import Paneel from "../generic/Paneel"

export type VorigeOnderzoekenConfirmBtnKind = "Onderzoek starten" | "Volgende";

export type VorigeOnderzoekenViewStateProps = {
	afspraak: Afspraak;
	client: Client;
	magOnderzoeken: boolean;
	gebruikersnaam?: string;
	setHeeftOudeBeeldenOpgevraagd: () => void;
};

export type VorigeOnderzoekenViewDispatchProps = {
	onConfirm: (client: Client, afspraak: Afspraak) => void;
};

export default class VorigeOnderzoekenView extends Component<VorigeOnderzoekenViewStateProps & VorigeOnderzoekenViewDispatchProps> {

	render(): JSX.Element {
		const client = this.props.client
		const afspraak = this.props.afspraak
		const alleVorigeOnderzoekenAanwezig = Math.min(afspraak.aantalOpgekomen, 3) === this.props.client.vorigeOnderzoeken.length
		return <div className="tabpagina">
			<div className="row">
				<div className="onderzoek-heading">
					<h1 className="float-left">Vorige onderzoeken</h1>
					<AutorisatieButton id="vorigeOnderzoekenButton"
									   label={vorigeOnderzoekenConfirmBtnKind(this.props.afspraak)}
									   heeftRecht={this.props.magOnderzoeken} rechtNaam={"Onderzoek starten op SE."}
									   onClick={(): void => {
										   this.props.onConfirm(client, afspraak)
									   }}>
					</AutorisatieButton>
				</div>
			</div>
			<div className="row">
				<PaspoortContainer client={client} afspraak={afspraak}/>
			</div>
			{alleVorigeOnderzoekenAanwezig ? null : <div className="row" key={"OnderzoekInformatieOntbreekt"}>
				<Paneel className={"onderzoek-component paneel-shadow"}>
					Door een technische fout kan de vorige onderzoeksinformatie (gedeeltelijk) niet weergegeven worden.
					U kunt het onderzoek normaal verder uitvoeren
				</Paneel>
			</div>}
			{this.props.client.vorigeOnderzoeken.map((vorigOnderzoek: VorigOnderzoek, index: number) => {
				return <div className="row" key={index}>
					<VorigOnderzoekContainer vorigOnderzoek={vorigOnderzoek} meestRecent={index === 0}
											 client={this.props.client} gebruikersnaam={this.props.gebruikersnaam}
											 setHeeftOudeBeeldenOpgevraagd={this.props.setHeeftOudeBeeldenOpgevraagd}/>
				</div>
			})}
		</div>
	}

}