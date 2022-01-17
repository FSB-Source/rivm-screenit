import React, {Component} from "react"
import {Col} from "reactstrap"
import type {Adres} from "../../../datatypes/Adres"
import {postcodeMetSpatie} from "../../../datatypes/Adres"
import SingleValue from "../../generic/SingleValue"

type GbaAdresViewProps = {
	adres: Adres;
	className?: string;
};

export default class GbaAdresView extends Component<GbaAdresViewProps> {
	render(): JSX.Element {
		const adres = this.props.adres
		const postcodeEnPlaats = `${postcodeMetSpatie(adres.postcode)} ${adres.plaats ? adres.plaats : ""}`
		return <Col className={this.props.className}>
			<SingleValue value={adres.locatieBeschrijving}/>
			<SingleValue value={postcodeEnPlaats}/>
		</Col>
	}

}