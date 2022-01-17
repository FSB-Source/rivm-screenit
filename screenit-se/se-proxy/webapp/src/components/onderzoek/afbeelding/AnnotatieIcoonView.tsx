import React, {ChangeEvent, Component} from "react"
import type {AnnotatieIcoon, AnnotatieIcoonType} from "../../../datatypes/AnnotatieIcoon"
import {
	convertXCoordinateToXPixels,
	convertXCoordinateToXPixelsOriginRightUpperCorner,
	convertXPixelsToXCoordinate,
	convertXPixelsToXCoordinateOriginRightUpperCorner,
	convertYCoordinateToYPixels,
	convertYCoordinateToYPixelsOriginRightUpperCorner,
	convertYPixelsToYCoordinate,
	convertYPixelsToYCoordinateOriginRightUpperCorner,
} from "../../../util/CoordinatenCalculator"
import Draggable, {DraggableEvent} from "react-draggable"
import {getAfbeeldingByType} from "../../../util/IcoonAfbeeldingTypeUtil"
import type {IcoonAfbeelding} from "../../../datatypes/IcoonAfbeelding"
import type {Aanzicht} from "./AnnotatieIcoonContainer"
import {getMandatory} from "../../../util/MapUtil"
import {annotatieTitle} from "../../../util/StringUtil"

const tekstvakbreedte = 130
export type Aanzichtpixels = {
	offsetX: number;
	offsetY: number;
	width: number;
	height: number;
};

export type AnnotatieIcoonViewStateProps = {
	afspraakId?: number;
	icoon: AnnotatieIcoon;
	icoonWidth: number;
	icoonHeight: number;
	imageWidth: number;
	imageHeight: number;
	isOpPalet?: boolean;
	aanzicht?: Aanzicht;
	aanzichten?: Map<Aanzicht, Aanzichtpixels>;
	metTextbox: boolean;
	isNietVisueleInspectie: boolean;
	isDraggable?: boolean;
	amputatieSize: number;
}

export type AnnotatieIcoonViewDispatchProps = {
	verwijderIcoon: (afspraakId: number, icoonId: number, xPos: number, yPos: number, aanzicht: Aanzicht | undefined) => void;
	setPosition: (afspraakId: number, icoonId: number, xPos: number, yPos: number, aanzicht: Aanzicht | undefined) => void;
	maakIcoon: (afspraakId: number, xPos: number, yPos: number, icoonType: AnnotatieIcoonType, aanzicht: Aanzicht | undefined) => void;
	verwerkTextChange: (afspraakId: number, icoonId: number, newValue: string) => void;
}

type AnnotatieIcoonViewState = {
	isOutsideCanvas: boolean;
	hasBeenOnCanvas: boolean;
};

type Coordinates = {
	x: number;
	y: number;
	aanzicht?: Aanzicht;
};

export default class AnnotatieIcoonView extends Component<AnnotatieIcoonViewStateProps & AnnotatieIcoonViewDispatchProps, AnnotatieIcoonViewState> {

	textboxRef: React.RefObject<HTMLInputElement>

	constructor(props: AnnotatieIcoonViewStateProps & AnnotatieIcoonViewDispatchProps) {
		super(props)
		this.state = {
			isOutsideCanvas: false,
			hasBeenOnCanvas: this.props.icoon.icoonId !== 0,
		}
		this.onStop = this.onStop.bind(this)
		this.textboxRef = React.createRef<HTMLInputElement>()
	}

	calcDropAanzicht = (x: number, y: number, aanzichten: Map<Aanzicht, Aanzichtpixels>): Aanzicht | undefined => {
		let aanzichtNaam: Aanzicht | undefined = undefined
		aanzichten.forEach((value, key) => {
			if (x > value.offsetX && x < value.offsetX + value.width) {
				aanzichtNaam = key
				return key
			}
		})
		return aanzichtNaam
	}

	outsideCanvas = (coord: Coordinates): boolean => {
		let yBottom: number | undefined

		if (coord.aanzicht && this.props.aanzichten) {
			const aanzichtpixels: Aanzichtpixels = getMandatory(this.props.aanzichten, coord.aanzicht)
			yBottom = 100.0 * aanzichtpixels.height / aanzichtpixels.width
		} else {
			yBottom = 100.0 * this.props.imageHeight / this.props.imageWidth
		}

		const outsideLeft = coord.x < 0.0
		const outsideRight = coord.x > 100.0
		const outsideTop = coord.y < 0.0
		const outsideBottom = yBottom && coord.y > yBottom
		return !!(outsideLeft || outsideRight || outsideTop || outsideBottom)
	}

	onStop = (e: DraggableEvent, position: {
		x: number;
		y: number;
	}): void => {
		const coordinates = this.convertPixelsToCoordinates(position)

		if (coordinates && this.props.afspraakId) {
			if (this.props.icoon.icoonId) {
				if (this.outsideCanvas(coordinates)) {
					this.setState({
						isOutsideCanvas: true,
						hasBeenOnCanvas: false,
					})
					this.props.verwijderIcoon(this.props.afspraakId, this.props.icoon.icoonId, coordinates.x, coordinates.y, this.props.aanzicht)
					return
				} else {
					this.setState({
						isOutsideCanvas: false,
						hasBeenOnCanvas: true,
					})
					this.props.setPosition(this.props.afspraakId, this.props.icoon.icoonId, coordinates.x, coordinates.y, this.props.aanzicht)
				}
			} else if (!this.outsideCanvas(coordinates)) {
				this.setState({
					isOutsideCanvas: true,
					hasBeenOnCanvas: false,
				})

				if (this.props.aanzichten) {
					const aanzichtenChecked = this.props.aanzichten
					const aanzicht = this.calcDropAanzicht(position.x, position.y, aanzichtenChecked)

					if (aanzicht) {
						this.props.maakIcoon(this.props.afspraakId, coordinates.x, coordinates.y, this.props.icoon.type, aanzicht)
					}
				} else {
					this.props.maakIcoon(this.props.afspraakId, coordinates.x, coordinates.y, this.props.icoon.type, undefined)
				}
			} else {
				this.setState({
					isOutsideCanvas: true,
					hasBeenOnCanvas: false,
				})
			}
		} else {
			this.setState({
				isOutsideCanvas: true,
				hasBeenOnCanvas: false,
			})
		}
	}

	convertPixelsToCoordinates = (position: {
		x: number;
		y: number;
	}): Coordinates | undefined => {
		const icoonAfbeelding = getAfbeeldingByType(this.props.icoon.type)
		let aanzicht: Aanzicht | undefined = undefined
		let aanzichtenChecked: Map<Aanzicht, Aanzichtpixels> | undefined = undefined

		if (this.props.aanzichten) {
			aanzichtenChecked = this.props.aanzichten
			aanzicht = this.calcDropAanzicht(position.x, position.y, aanzichtenChecked)
		}

		if (this.props.icoon.icoonId) {
			return this.getConvertedCoordinatesFromPixels(icoonAfbeelding, position, aanzicht)
		} else {
			if (this.props.aanzichten) {
				if (aanzicht && aanzichtenChecked) {
					return {
						x: convertXPixelsToXCoordinate(position.x - getMandatory(aanzichtenChecked, aanzicht).offsetX, getMandatory(aanzichtenChecked, aanzicht).width, this.icoonWidth()),
						y: convertYPixelsToYCoordinate(this.props.imageHeight + position.y, getMandatory(aanzichtenChecked, aanzicht).width, this.icoonHeight()),
						aanzicht: aanzicht,
					}
				} else {
					return undefined
				}
			} else {
				return this.getConvertedCoordinatesFromPixels(icoonAfbeelding, position, undefined)
			}
		}
	}

	onDrag = (e: DraggableEvent, position: {
		x: number;
		y: number;
	}): void => {
		const coordinates = this.convertPixelsToCoordinates(position)

		if (coordinates) {
			const draggedIconOutsideCanvas: boolean = this.outsideCanvas(coordinates)

			if (draggedIconOutsideCanvas) {
				this.onStop(e, position)
			}

			this.setState({
				isOutsideCanvas: draggedIconOutsideCanvas,
				hasBeenOnCanvas: this.state.hasBeenOnCanvas || !draggedIconOutsideCanvas,
			})
		}
	}

	textChange = (event: ChangeEvent<HTMLInputElement>): void => {
		const target = event.target

		if (target instanceof HTMLInputElement && this.props.afspraakId) {
			this.props.verwerkTextChange(this.props.afspraakId, this.props.icoon.icoonId, target.value)
		}
	}

	componentDidMount(): void {
		if (this.textboxRef && this.textboxRef.current && this.props.isDraggable && this.props.metTextbox && this.props.icoon.nieuwIcoon) {
			this.textboxRef.current.focus()
		}
	}

	render(): JSX.Element {
		const icoon = this.props.icoon
		const icoonAfbeelding: IcoonAfbeelding = getAfbeeldingByType(icoon.type, this.props.isNietVisueleInspectie)
		return <Draggable position={this.getPixelsFromCoordinates(icoonAfbeelding)} onStop={this.onStop}
						  onDrag={this.onDrag} enableUserSelectHack={false} disabled={!this.props.isDraggable}>
			{this.state.isOutsideCanvas && this.state.hasBeenOnCanvas ? <div/> :
				<div className={`visuele-inspectie-icoon${this.notSelectable()}`} title={annotatieTitle(icoon.type)}
					 style={{
						 backgroundImage: `url(${icoonAfbeelding.afbeelding})`,
						 backgroundRepeat: "no-repeat",
						 backgroundSize: "cover",
						 width: this.icoonWidth(),
						 height: this.icoonHeight(),
					 }}>
					{this.props.metTextbox &&
					<input
						ref={this.textboxRef} type="text" disabled={!this.props.isDraggable}
						defaultValue={icoon.tekst} maxLength={12}
						className={`icoontext${this.notSelectable()}`}
						onChange={this.textChange}
						style={{
							marginTop: this.icoonHeight(),
							width: tekstvakbreedte,
							marginLeft: (this.icoonWidth() - tekstvakbreedte) / 2,
						}}/>}
				</div>}
		</Draggable>
	}

	icoonWidth(): number {
		const icoon = this.props.icoon
		return icoon.icoonId && icoon.type === "AMPUTATIE" ? this.props.amputatieSize : getAfbeeldingByType(icoon.type).width
	}

	icoonHeight(): number {
		const icoon = this.props.icoon
		return icoon.icoonId && icoon.type === "AMPUTATIE" ? this.props.amputatieSize : getAfbeeldingByType(icoon.type).height
	}

	getPixelsFromCoordinates(icoonAfbeelding: IcoonAfbeelding): Coordinates {
		if (icoonAfbeelding.isRightUpperCornerOrigin === true && this.props.isOpPalet !== true) {
			return {
				x: convertXCoordinateToXPixelsOriginRightUpperCorner(this.props.icoon.positieX, this.props.imageWidth, this.icoonWidth()),
				y: convertYCoordinateToYPixelsOriginRightUpperCorner(this.props.icoon.positieY, this.props.imageWidth) - this.props.imageHeight,
			}
		} else {
			return {
				x: convertXCoordinateToXPixels(this.props.icoon.positieX, this.props.imageWidth, this.icoonWidth()),
				y: convertYCoordinateToYPixels(this.props.icoon.positieY, this.props.imageWidth, this.icoonHeight()) - this.props.imageHeight,
			}
		}
	}

	getConvertedCoordinatesFromPixels(icoonAfbeelding: IcoonAfbeelding, position: {
		x: number;
		y: number;
	}, aanzicht: Aanzicht | undefined): Coordinates | undefined {
		if (icoonAfbeelding.isRightUpperCornerOrigin === true) {
			return {
				x: convertXPixelsToXCoordinateOriginRightUpperCorner(position.x, this.props.imageWidth, this.icoonWidth()),
				y: convertYPixelsToYCoordinateOriginRightUpperCorner(this.props.imageHeight + position.y, this.props.imageWidth),
				aanzicht: aanzicht,
			}
		} else {
			return {
				x: convertXPixelsToXCoordinate(position.x, this.props.imageWidth, this.icoonWidth()),
				y: convertYPixelsToYCoordinate(this.props.imageHeight + position.y, this.props.imageWidth, this.icoonHeight()),
				aanzicht: aanzicht,
			}
		}
	}

	notSelectable(): string {
		return !this.props.isDraggable ? " no-select" : ""
	}

}