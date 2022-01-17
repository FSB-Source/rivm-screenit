import React, {Component} from "react"
import AfwijkingenAfbeeldingenView from "./AfwijkingenAfbeeldingenView"
import {Row} from "reactstrap"
import type {AnnotatieIcoon} from "../../../datatypes/AnnotatieIcoon"
import type {Aanzicht} from "../afbeelding/AnnotatieIcoonContainer"
import type {DoorsnedeAfbeeldingen} from "../../../datatypes/DoorsnedeAfbeeldingen"
import type {Amputatie} from "../../../datatypes/Onderzoek"

export type AfwijkingenContainerProps = {
	afspraakId?: number;
	blokId: string;
	isEditable?: boolean;
	lezingAanzichten?: DoorsnedeAfbeeldingen;
	amputatie?: Amputatie;
};

export type AfwijkingenContainerState = {
	width: number;
	height: number;
	aanzichten: Map<Aanzicht, {
		offsetX: number;
		offsetY: number;
		width: number;
		height: number;
	}>;
};

export default class AfwijkingenContainerView extends Component<AfwijkingenContainerProps, AfwijkingenContainerState> {

	linksHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>
	rechtsHorizontaleDoorsnedeIconen: Map<number, AnnotatieIcoon>
	linksVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>
	rechtsVerticaleDoorsnedeIconen: Map<number, AnnotatieIcoon>

	constructor(props: AfwijkingenContainerProps) {
		super(props)
		this.linksHorizontaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.linksHorizontaleDoorsnede ? this.props.lezingAanzichten.linksHorizontaleDoorsnede.iconenById : new Map()
		this.rechtsHorizontaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.rechtsHorizontaleDoorsnede ? this.props.lezingAanzichten.rechtsHorizontaleDoorsnede.iconenById : new Map()
		this.linksVerticaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.linksVerticaleDoorsnede ? this.props.lezingAanzichten.linksVerticaleDoorsnede.iconenById : new Map()
		this.rechtsVerticaleDoorsnedeIconen = this.props.lezingAanzichten && this.props.lezingAanzichten.rechtsVerticaleDoorsnede ? this.props.lezingAanzichten.rechtsVerticaleDoorsnede.iconenById : new Map()
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
		return <Row noGutters id={`vorig-onderzoek-afwijking${this.props.blokId}`}>
			<AfwijkingenAfbeeldingenView afspraakId={this.props.afspraakId} afwijkingenAfbeeldingId={this.props.blokId}
										 iconenByIdRechtsVerticaal={this.rechtsVerticaleDoorsnedeIconen}
										 iconenByIdLinksVerticaal={this.linksVerticaleDoorsnedeIconen}
										 iconenByIdRechtsHorizontaal={this.rechtsHorizontaleDoorsnedeIconen}
										 iconenByIdLinksHorizontaal={this.linksHorizontaleDoorsnedeIconen}
										 onLoad={this.updateParentState} isEditable={this.props.isEditable}
										 isNietVisueleInspectie={true} amputatie={this.props.amputatie}/>
		</Row>
	}

}
