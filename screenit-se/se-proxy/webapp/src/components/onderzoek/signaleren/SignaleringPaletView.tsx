import React, {Component} from "react"
import type {AnnotatieIcoonType} from "../../../datatypes/AnnotatieIcoon"
import {getAfbeeldingByType} from "../../../util/IcoonAfbeeldingTypeUtil"
import AnnotatiePaletView from "../afbeelding/AnnotatiePaletView"
import {Col, Row} from "reactstrap"
import type {Aanzicht} from "../afbeelding/AnnotatieIcoonContainer"
import AfwijkingenAfbeeldingenView from "./AfwijkingenAfbeeldingenView"

export type PaletProps = {
	afspraakId: number;
	afwijkingenAfbeeldingId: string;
	paletIconen: Array<AnnotatieIcoonType>;
	imageWidth: number;
	imageHeight: number;
	isEditable: boolean;
	aanzichten: Map<Aanzicht, {
		offsetX: number;
		offsetY: number;
		width: number;
		height: number;
	}>;
};

const magicYOffset = 2

export default class SignaleringPaletView extends Component<PaletProps> {

	getMaxIconHeight(): number {
		return Math.max(...this.props.paletIconen.map((value: AnnotatieIcoonType) => {
			return getAfbeeldingByType(value).height
		}))
	}

	render(): JSX.Element {
		return <Row noGutters>
			<Col>
				<AnnotatiePaletView afspraakId={this.props.afspraakId} icoonList={this.props.paletIconen} imageWidth={this.props.imageWidth}
									imageHeight={this.props.imageHeight - AfwijkingenAfbeeldingenView.calcYOffset(this.props.afwijkingenAfbeeldingId) + magicYOffset}
									paletWidth={this.props.imageWidth / 3} paletHeight={this.getMaxIconHeight()} aanzichten={this.props.aanzichten}
									isEditable={this.props.isEditable}/>
			</Col>

		</Row>
	}

}