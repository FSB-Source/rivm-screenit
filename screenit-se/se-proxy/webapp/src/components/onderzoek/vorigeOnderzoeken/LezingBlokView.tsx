import React, {Component} from "react"
import AfwijkingenAfbeeldingenView from "../signaleren/AfwijkingenAfbeeldingenView"
import type {Lezing} from "../../../datatypes/Lezing"
import {Col} from "reactstrap"
import type {AnnotatieIcoon} from "../../../datatypes/AnnotatieIcoon"
import BiradsWaardeView from "./BiradsWaardeView"
import type {Aanzicht} from "../afbeelding/AnnotatieIcoonContainer"
import AfwijkingenContainerView from "../signaleren/AfwijkingenContainerView"
import Paneel from "../../generic/Paneel"

export type LezingBlokViewProps = {
	blokId: string;
	lezing: Lezing;
	colWidth: number;
};

export type LezingBlokViewState = {
	width: number;
	height: number;
	aanzichten: Map<Aanzicht, {
		offsetX: number;
		offsetY: number;
		width: number;
		height: number;
	}>;
};

export default class LezingBlokView extends Component<LezingBlokViewProps, LezingBlokViewState> {

	linksHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>
	rechtsHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>
	linksVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>
	rechtsVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>

	constructor(props: LezingBlokViewProps) {
		super(props)
		this.linksHorizontaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.linksHorizontaleDoorsnede ? this.props.lezing.lezingAanzichten.linksHorizontaleDoorsnede.iconenById : new Map()
		this.rechtsHorizontaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.rechtsHorizontaleDoorsnede ? this.props.lezing.lezingAanzichten.rechtsHorizontaleDoorsnede.iconenById : new Map()
		this.linksVerticaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.linksVerticaleDoorsnede ? this.props.lezing.lezingAanzichten.linksVerticaleDoorsnede.iconenById : new Map()
		this.rechtsVerticaleDoorsnedeIconen = this.props.lezing.lezingAanzichten && this.props.lezing.lezingAanzichten.rechtsVerticaleDoorsnede ? this.props.lezing.lezingAanzichten.rechtsVerticaleDoorsnede.iconenById : new Map()
		this.updateParentState.bind(this)
	}

	updateParentState = (): void => {
		const element = document && document.getElementById(`vorig-onderzoek-afwijking${this.props.blokId}`)
		if (element) {
			this.setState({
				width: element.clientWidth,
				height: element.clientHeight,
				aanzichten: AfwijkingenAfbeeldingenView.getRenderedAanzichten(this.props.blokId),
			})
		}
	}

	render(): JSX.Element {
		try {
			return <Col md={this.props.colWidth} className={"px-2"}>
				<div className={"vorig-onderzoek-lezing-blok"}>
					<AfwijkingenContainerView blokId={this.props.blokId}
											  lezingAanzichten={this.props.lezing.lezingAanzichten} isEditable={false}
											  amputatie={this.props.lezing.vorigOnderzoek.onderzoek.amputatie}/>
					<BiradsWaardeView lezing={this.props.lezing}/>
				</div>
			</Col>
		} catch (exception: any) {
			console.warn(`fout tijdens aanmaken van lezing: ${exception.message}`)
			return <Paneel className={"onderzoek-component paneel-shadow"}>Door een technische fout kan de vorige lezing
				niet correct worden ingeladen.</Paneel>
		}
	}

}
