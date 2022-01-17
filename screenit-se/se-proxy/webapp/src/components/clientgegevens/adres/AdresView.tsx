import React, {Component} from "react"
import {Button, Col, Row} from "reactstrap"
import type {Adres} from "../../../datatypes/Adres"
import Paneel from "../../generic/Paneel"
import type {TijdelijkAdres} from "../../../datatypes/TijdelijkAdres"
import "font-awesome/css/font-awesome.min.css"
import TijdelijkGbaAdresTitelView from "./TijdelijkGbaAdresTitelView"
import AdresTitelView from "./AdresTitelView"
import GbaAdresView from "./GbaAdresView"
import TijdelijkAdresView from "./TijdelijkAdresView"
import TijdelijkAdresWijzigenView from "./TijdelijkAdresWijzigenView"

export type AdresViewProps = {
	adres: Adres;
	tijdelijkGbaAdres?: Adres;
	tijdelijkAdres?: TijdelijkAdres;
	disabled: boolean;
	clientId: number;
};

export type AdresViewState = {
	toonTijdelijkAdresWijzigenPopUp: boolean;
};

export default class AdresView extends Component<AdresViewProps, AdresViewState> {

	constructor(props: AdresViewProps) {
		super(props)
		this.state = {
			toonTijdelijkAdresWijzigenPopUp: false,
		}
		this.toggleTijdelijkAdresWijzigen = this.toggleTijdelijkAdresWijzigen.bind(this)
	}

	toggleTijdelijkAdresWijzigen = (): void => {
		this.setState({
			toonTijdelijkAdresWijzigenPopUp: !this.state.toonTijdelijkAdresWijzigenPopUp,
		})
	}

	render(): JSX.Element {
		return <Paneel>
			<Row>
				<AdresTitelView adresTitel={"Adres"} className={"col-6"}/>
				{this.props.tijdelijkGbaAdres ? <TijdelijkGbaAdresTitelView/> : null}
				<AdresTitelView adresTitel={"Tijdelijk adres"} className={"col-4"}/>
				<TijdelijkAdresWijzigenView
					isOpen={this.state.toonTijdelijkAdresWijzigenPopUp}
					toggle={this.toggleTijdelijkAdresWijzigen}
					clientId={this.props.clientId}/>
				<div className={"col-2"}>
					{!this.props.disabled &&
					<Button className={"float-right btn-secondary-se"} onClick={this.toggleTijdelijkAdresWijzigen}>Wijzig
						tijdelijk adres</Button>}
				</div>
			</Row>
			<Row>
				<GbaAdresView adres={this.props.adres} className={"col-6"}/>
				{this.props.tijdelijkGbaAdres ?
					<GbaAdresView adres={this.props.tijdelijkGbaAdres} className={"col-6"}/> : null}
				{this.props.tijdelijkAdres ?
					<TijdelijkAdresView
						tijdelijkAdres={this.props.tijdelijkAdres}
						disabled={this.props.disabled}
						className={"col-4"}/> : <Col/>}
				<div className={"col-2"}/>
			</Row>
		</Paneel>
	}

}
