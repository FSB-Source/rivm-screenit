import React, {Component} from "react"
import type {AnnotatieIcoonType} from "../../../datatypes/AnnotatieIcoon"
import {convertXPixelsToXCoordinate, convertYPixelsToYCoordinate} from "../../../util/CoordinatenCalculator"
import type {Aanzicht} from "./AnnotatieIcoonContainer"
import AnnotatieIcoonContainer from "./AnnotatieIcoonContainer"
import {getAfbeeldingByType} from "../../../util/IcoonAfbeeldingTypeUtil"

export type AnnotatiePaletProps = {
	afspraakId: number;
	icoonList: Array<AnnotatieIcoonType>;
	imageWidth: number;
	imageHeight: number;
	paletWidth?: number;
	paletHeight: number;
	isEditable?: boolean;
	aanzichten?: Map<Aanzicht, {
		offsetX: number;
		offsetY: number;
		width: number;
		height: number;
	}>;
};
const yPadding = 5

export default class AnnotatiePaletView extends Component<AnnotatiePaletProps> {

	render(): JSX.Element {
		return <div style={{
			height: `${this.props.paletHeight + yPadding}px`,
			paddingBottom: `${yPadding}px`,
		}}>
			{this.props.icoonList.map((icoonType: AnnotatieIcoonType) => {
				return <AnnotatieIcoonContainer
					{...this.props}
					key={icoonType}
					isOpPalet={true}
					metTextbox={false}
					isDraggable={this.props.isEditable}
					icoon={{
						icoonId: 0,
						positieX: this.getScaledX(this.props.icoonList, icoonType),
						positieY: this.getScaledYOffset(icoonType),
						type: icoonType,
						nieuwIcoon: false,
					}}/>
			})}
		</div>
	}

	getScaledX(iconen: Array<AnnotatieIcoonType>, icoon: AnnotatieIcoonType): number {
		const icoonWidth = getAfbeeldingByType(icoon).width
		const spacing = (this.props.paletWidth ? this.props.paletWidth : this.props.imageWidth) / iconen.length
		const spacingMargin = spacing / iconen.length
		const pixelWidth = iconen.slice(0, iconen.indexOf(icoon)).reduce(accumulator => accumulator + spacing, spacingMargin)
		return convertXPixelsToXCoordinate(pixelWidth + (this.props.paletWidth ? this.props.paletWidth : 0), this.props.imageWidth, spacing - icoonWidth / 2)
	}

	getScaledYOffset(icoon: AnnotatieIcoonType): number {
		const icoonHeight = getAfbeeldingByType(icoon).height
		return convertYPixelsToYCoordinate(yPadding * 2 + this.props.imageHeight, this.props.imageWidth, this.props.paletHeight - icoonHeight / 2)
	}

}