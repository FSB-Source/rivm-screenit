import React, {Component} from "react"
import type {GeenHuisartsOption, Huisarts} from "../../../datatypes/Huisarts"
import {getGeenHuisartsLabel, getHuisartsVolledigAdres} from "../../../datatypes/Huisarts"
import LabelValue from "../../generic/LabelValue"
import {Button, Col, Row} from "reactstrap"
import PaneelNaam from "../../generic/PaneelNaam"
import Paneel from "../../generic/Paneel"
import type {Form} from "../../../datatypes/Form"
import {Afspraak} from "../../../datatypes/Afspraak"
import HuisartsZoekenView from "./HuisartsZoekenView"

type HuisartsViewState = {
	toonHuisartsZoekenPopup: boolean;
};

type HuisartsgegevensProps = {
	huisarts: Huisarts;
};

type GeenHuisartsProps = {
	geenHuisartsOptie?: GeenHuisartsOption;
};

export type HuisartsViewStateProps = GeenHuisartsProps & {
	afspraak: Afspraak;
	huisarts?: Huisarts;
	clientGegevensForm: Form;
	isValid: boolean;
	disabled: boolean;
	clientPlaats: string;
};

export type HuisartsViewDispatchProps = {
	onKiesHuisarts: (afspraak: Afspraak, huisarts: Huisarts) => void;
	onKiesGeenHuisartsOptie: (afspraak: Afspraak, geenHuisartsOptie: GeenHuisartsOption) => void;
}
export default class HuisartsView extends Component<HuisartsViewStateProps & HuisartsViewDispatchProps, HuisartsViewState> {

	constructor(props: HuisartsViewStateProps & HuisartsViewDispatchProps) {
		super(props)
		this.state = {
			toonHuisartsZoekenPopup: false,
		}
		this.toggleHuisartsZoekenPopup = this.toggleHuisartsZoekenPopup.bind(this)
		this.selecteerHuisarts = this.selecteerHuisarts.bind(this)
		this.selecteerGeenHuisarts = this.selecteerGeenHuisarts.bind(this)
	}

	toggleHuisartsZoekenPopup = (): void => {
		this.setState({
			toonHuisartsZoekenPopup: !this.state.toonHuisartsZoekenPopup,
		})
	}

	selecteerHuisarts = (huisarts: Huisarts): void => {
		this.props.onKiesHuisarts(this.props.afspraak, huisarts)
		this.toggleHuisartsZoekenPopup()
	}

	selecteerGeenHuisarts = (geenHuisartsOptie: GeenHuisartsOption): void => {
		this.props.onKiesGeenHuisartsOptie(this.props.afspraak, geenHuisartsOptie)
		this.toggleHuisartsZoekenPopup()
	}

	render(): JSX.Element {
		const huisarts = this.props.huisarts
		const geenHuisartsOptie = this.props.geenHuisartsOptie
		return <Paneel>
			<PaneelNaam titel={"Huisarts*"}>
				{!this.props.disabled &&
				<Button className={"float-right btn-secondary-se"} onClick={this.toggleHuisartsZoekenPopup}>Zoek
					huisarts</Button>}
			</PaneelNaam>
			{!this.props.isValid && <div className={"invalid-feedback"} style={{
				display: "block",
			}}>Het selecteren van een huisarts of één van de andere opties is verplicht.</div>}
			{huisarts ? <HuisartsgegevensView huisarts={huisarts}/> :
				<GeenHuisartsView geenHuisartsOptie={geenHuisartsOptie}/>}
			<HuisartsZoekenView
				clientPlaats={this.props.clientPlaats} geenHuisartsOptie={geenHuisartsOptie}
				selecteerHuisarts={this.selecteerHuisarts}
				selecteerGeenHuisarts={this.selecteerGeenHuisarts} afspraak={this.props.afspraak}
				huisarts={huisarts} isOpen={this.state.toonHuisartsZoekenPopup}
				toggle={this.toggleHuisartsZoekenPopup}/>
		</Paneel>
	}

}

const HuisartsgegevensView = (huisartsProps: HuisartsgegevensProps): JSX.Element => {
	const huisarts = huisartsProps.huisarts
	return <Row noGutters>
		<Col md={6}>
			<LabelValue label={"Naam huisarts"} mdLabel={4} mdValue={8} value={huisarts.naamHuisarts}/>
			<LabelValue label={"Weergave naam"} mdLabel={4} mdValue={8} value={huisarts.weergaveNaam}/>
			<LabelValue label={"Praktijknaam"} mdLabel={4} mdValue={8} value={huisarts.praktijknaam}/>
		</Col>
		<Col md={6}>
			<LabelValue label={"Adres"} mdLabel={4} mdValue={8} value={getHuisartsVolledigAdres(huisarts)}/>
		</Col>
	</Row>
}

const GeenHuisartsView = (huisartsProps: GeenHuisartsProps): JSX.Element => {
	const geenHuisartsOptie = huisartsProps.geenHuisartsOptie
	return <Row>
		<Col md={6}>
			{geenHuisartsOptie && <LabelValue mdLabel={4} mdValue={8} label={"Anders"}
											  value={getGeenHuisartsLabel(huisartsProps.geenHuisartsOptie)}/>}
		</Col>
	</Row>
}