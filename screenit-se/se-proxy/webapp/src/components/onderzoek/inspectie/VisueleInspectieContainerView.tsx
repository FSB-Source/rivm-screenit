import React, {Component} from "react"
import type {AnnotatieIcoon, AnnotatieIcoonType} from "../../../datatypes/AnnotatieIcoon"
import * as CoordinationCalculator from "../../../util/CoordinatenCalculator"
import VisueleInspectieAfbeeldingView from "./VisueleInspectieAfbeeldingView"
import type {Amputatie} from "../../../datatypes/Onderzoek"

export type VisueleInspectieContainerViewProps = {
	containerId: string;
	afspraakId: number;
	iconenById: Map<number, AnnotatieIcoon>;
	paletIconen: Array<AnnotatieIcoonType>;
	isEditable?: boolean;
	noGutters?: boolean;
	amputatie?: Amputatie;
};

export type VisueleInspectieContainerViewState = {
	width: number;
	height: number;
};

export default class VisueleInspectieContainerView extends Component<VisueleInspectieContainerViewProps, VisueleInspectieContainerViewState> {
	constructor(props: VisueleInspectieContainerViewProps) {
		super(props)
		this.afterLoad.bind(this)
		this.state = {
			width: 0,
			height: 0,
		}
	}

	afterLoad = (): void => {
		this.setState({
			width: CoordinationCalculator.getWidth(`visuele-inspectie-afbeelding${this.props.containerId}`),
			height: CoordinationCalculator.getHeight(`visuele-inspectie-afbeelding${this.props.containerId}`),
		})
	}

	render(): JSX.Element {
		return <VisueleInspectieAfbeeldingView
			containerId={this.props.containerId} width={this.state.width}
			height={this.state.height} afspraakId={this.props.afspraakId}
			iconenById={this.props.iconenById} paletIconen={[]}
			noGutters={this.props.noGutters} afterLoad={this.afterLoad}
			isEditable={this.props.isEditable} amputatie={this.props.amputatie}/>
	}

}