import React, {Component} from "react"
import type {VorigOnderzoek} from "../../../datatypes/VorigOnderzoek"
import {Button} from "reactstrap"
import type {Client} from "../../../datatypes/Client"
import VorigOnderzoekUitklapBalkView from "./VorigOnderzoekUitklapBalk"
import LezingenBlokView from "./LezingenBlokView"
import OnderzoekBlokView from "./OnderzoekBlokView"
import Paneel from "../../generic/Paneel"

export type VorigOnderzoekViewStateProps = {
	vorigOnderzoek: VorigOnderzoek;
	meestRecent: boolean;
	client: Client;
	gebruikersnaam?: string;
	setHeeftOudeBeeldenOpgevraagd: () => void;
};

export type VorigOnderzoekViewDispatchProps = {
	vorigeOnderzoekOphalen: (clientId: number, uitnodigingsNr: number, gebruikersnaam: string, bsn: string) => void;
}

type VorigOnderzoekState = {
	opengeklapt: boolean;
};

export default class VorigOnderzoekView extends Component<VorigOnderzoekViewStateProps & VorigOnderzoekViewDispatchProps, VorigOnderzoekState> {

	constructor(props: VorigOnderzoekViewStateProps & VorigOnderzoekViewDispatchProps) {
		super(props)
		this.props.setHeeftOudeBeeldenOpgevraagd.bind(this)
		this.state = {
			opengeklapt: props.meestRecent,
		}
	}

	toggleOpengeklapt = (): void => {
		this.setState({
			opengeklapt: !this.state.opengeklapt,
		})
	}

	render(): JSX.Element {
		try {
			return this.state.opengeklapt ? <div className="vorig-onderzoek-opengeklapt vorig-onderzoek">
				<VorigOnderzoekUitklapBalkView
					eersteBeeindigdeAfspraakOp={this.props.vorigOnderzoek.eersteBeeindigdeAfspraakOp}
					toggleOpengeklapt={this.toggleOpengeklapt} opgenklapt={this.state.opengeklapt}
					uitslagGunstig={this.props.vorigOnderzoek.uitslagGunstig}/>
				<div className="vorig-onderzoek p-2 px-4">
					<OnderzoekBlokView vorigOnderzoek={this.props.vorigOnderzoek}/>
					<LezingenBlokView vorigOnderzoek={this.props.vorigOnderzoek}/>
					{(!this.props.meestRecent && this.props.vorigOnderzoek.beeldenBeschikbaar) &&
					<Button color={"link"} className={"float-right gray-link"} onClick={(): void => {
						if (this.props.gebruikersnaam) {
							this.props.vorigeOnderzoekOphalen(this.props.client.id, this.props.vorigOnderzoek.uitnodigingsNr, this.props.gebruikersnaam, this.props.client.bsn)
							this.props.setHeeftOudeBeeldenOpgevraagd()
						}
					}}>Ophalen beelden</Button>}
				</div>
			</div> : <VorigOnderzoekUitklapBalkView
				eersteBeeindigdeAfspraakOp={this.props.vorigOnderzoek.eersteBeeindigdeAfspraakOp}
				toggleOpengeklapt={this.toggleOpengeklapt} opgenklapt={this.state.opengeklapt}
				uitslagGunstig={this.props.vorigOnderzoek.uitslagGunstig}/>
		} catch (exception: any) {
			console.warn(`fout tijdens aanmaken van vorig onderzoek view: ${exception.message}`)
			return <Paneel className={"onderzoek-component paneel-shadow"}>Door een technische fout kan de vorige
				onderzoeksinformatie (gedeeltelijk) niet weergegeven worden. U kunt het onderzoek
				normaal verder uitvoeren.</Paneel>
		}
	}

}