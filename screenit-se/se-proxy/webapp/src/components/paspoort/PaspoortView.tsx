import React, {Component} from "react"
import type {Client} from "../../datatypes/Client"
import {datumFormaat} from "../../util/DateUtil"
import {Col, Row} from "reactstrap"
import {postcodeMetSpatie} from "../../datatypes/Adres"
import LabelValue from "../generic/LabelValue"
import Paneel from "../generic/Paneel"
import PaneelNaam from "../generic/PaneelNaam"
import type {Afspraak} from "../../datatypes/Afspraak"
import {AANVULLENDE_BEELDEN_NODIG_SE} from "../../datatypes/OpschortenReden"

export type PaspoortProps = {
	client: Client;
	afspraak: Afspraak;
	tijdelijkAdresValue: string;
};
export default class PaspoortView extends Component<PaspoortProps> {

	render(): JSX.Element {
		const client = this.props.client
		const afspraak = this.props.afspraak
		return <Paneel className="paspoort">
			<PaneelNaam className="paspoort-clientnaam" titel={`${client.voorletters} ${client.aanspreekTussenvoegselEnAchternaam}`}>
				<div>
					{this.props.client.inTehuis && <i className="fa fa-home px-1 py-1 float-right"/>}
					{!this.props.client.inTehuis && this.props.client.doelgroep === "DUBBELE_TIJD" &&
					<i className="fa fa-clock-o px-1 py-1 float-right"/>}
					{this.props.client.doelgroep === "MINDER_VALIDE" &&
					<i className="fa fa-wheelchair px-1 py-1 float-right"/>}
					{this.props.afspraak.eerderOnderbrokenInZelfdeRonde ?
						<i className="fa fa-step-forward px-1 py-1 float-right"/> : null}
					{this.props.afspraak.eerdereOpschortenReden === AANVULLENDE_BEELDEN_NODIG_SE ?
						<i className="fa fa-plus px-1 py-1 float-right"/> : null}
					{this.props.afspraak.geforceerd ? <i className="fa fa-plus px-1 py-1 float-right"/> : null}
				</div>
			</PaneelNaam>

			<Row md={12}>
				<Col sm={6}>
					<LabelValue label="Burgerservicenummer" value={client.bsn}/>
					<LabelValue label="Geboortedatum" value={datumFormaat(client.geboortedatum)}/>
					<LabelValue label="Geboortenaam" value={(client.geboorteTussenvoegsel ? `${client.geboorteTussenvoegsel} ` : "") + client.geboorteAchternaam}/>
					<LabelValue label="Uitnodigingsnummer" value={afspraak.uitnodigingsNr.toString()}/>
					<LabelValue label="Opgeroepen" value={afspraak.aantalOproepen.toString()}/>
				</Col>
				<Col sm={6}>
					<LabelValue label="Straat" value={client.adres.locatieBeschrijving}/>
					<LabelValue label="Postcode" value={postcodeMetSpatie(client.adres.postcode)}/>
					<LabelValue label="Plaats" value={client.adres.plaats}/>
					<LabelValue label="Tijdelijk adres" value={this.props.tijdelijkAdresValue}/>
					<LabelValue label="Opkomst" value={afspraak.aantalOpgekomen.toString()}/>
				</Col>
			</Row>
		</Paneel>
	}

}