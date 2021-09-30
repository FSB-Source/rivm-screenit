/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import React, {Component} from 'react';
import type {AnnotatieIcoon, AnnotatieIcoonType} from '../../../datatypes/AnnotatieIcoon';
import {
    convertXCoordinateToXPixels,
    convertXCoordinateToXPixelsOriginRightUpperCorner,
    convertXPixelsToXCoordinate,
    convertXPixelsToXCoordinateOriginRightUpperCorner,
    convertYCoordinateToYPixels,
    convertYCoordinateToYPixelsOriginRightUpperCorner,
    convertYPixelsToYCoordinate,
    convertYPixelsToYCoordinateOriginRightUpperCorner,
} from '../../../util/CoordinatenCalculator';
import Draggable from 'react-draggable';
import {getAfbeeldingByType} from '../../../util/IcoonAfbeeldingTypeUtil';
import type {IcoonAfbeelding} from '../../../datatypes/IcoonAfbeelding';
import type {Aanzicht} from './AnnotatieIcoonContainer';
import {getMandatory} from '../../../util/MapUtil';
import {annotatieTitle} from '../../../util/StringUtil';

const tekstvakbreedte = 130;

type Aanzichtpixels = { offsetX: number, offsetY: number, width: number, height: number };

export type AnnotatieIcoonProps = {
    afspraakId: number,
    icoon: AnnotatieIcoon,
    icoonWidth: number,
    icoonHeight: number,
    imageWidth: number,
    imageHeight: number,
    isOpPalet?: boolean,
    aanzicht?: Aanzicht | null,
    aanzichten?: Map<Aanzicht, Aanzichtpixels>,
    metTextbox: boolean,
    isNietVisueleInspectie: boolean,
    isDraggable?: boolean,
    amputatieSize: number,

    verwijderIcoon: (afspraakId: number, icoonId: number, xPos: number, yPos: number, aanzicht: Aanzicht | null) => mixed,
    setPosition: (afspraakId: number, icoonId: number, xPos: number, yPos: number, aanzicht: Aanzicht | null) => mixed,
    maakIcoon: (afspraakId: number, xPos: number, yPos: number, icoonType: AnnotatieIcoonType, aanzicht: Aanzicht | null) => mixed,
    getAfbeeldingBounds: (width: number, height: number, imageWidth: number, imageHeight: number) => mixed,
    verwerkTextChange: (afspraakId: number, icoonId: number, newValue: string) => mixed,
}

type AnnotatieIcoonState = {
    isOutsideCanvas: boolean,
    hasBeenOnCanvas: boolean
}

type Coordinates = {
    x: number,
    y: number,
    aanzicht: Aanzicht | null
}

export default class AnnotatieIcoonView extends Component<AnnotatieIcoonProps, AnnotatieIcoonState> {

    calcDropAanzicht = (x: number, y: number, aanzichten: Map<Aanzicht, Aanzichtpixels>): Aanzicht | null => {
        let aanzichtNaam: Aanzicht | null = null;
        aanzichten.forEach((value, key) => {
            if (x > value.offsetX && x < value.offsetX + value.width) {
                aanzichtNaam = key;
                return key;
            }
        });
        return aanzichtNaam;
    };

    outsideCanvas = (coord: Coordinates): boolean => {
        let yBottom: number | null = null;
        if (coord.aanzicht && this.props.aanzichten) {
            const aanzichtpixels: Aanzichtpixels = getMandatory(this.props.aanzichten, coord.aanzicht);
            yBottom = 100.0 * aanzichtpixels.height / aanzichtpixels.width;
        } else {
            yBottom = 100.0 * this.props.imageHeight / this.props.imageWidth;
        }
        const outsideLeft = coord.x < 0.0;
        const outsideRight = coord.x > 100.0;
        const outsideTop = coord.y < 0.0;
        const outsideBottom = (yBottom !== null && coord.y > yBottom);
        return (outsideLeft || outsideRight || outsideTop || outsideBottom);
    };

    onStop = (e: Event, position: { x: number, y: number }) => {
        const coordinates: Coordinates | null = this.convertPixelsToCoordinates(position);
        if (coordinates) {
            if (this.props.icoon.icoonId) { 
                if (this.outsideCanvas(coordinates)) { 
                    this.setState({isOutsideCanvas: true, hasBeenOnCanvas: false}); 
                    this.props.verwijderIcoon(this.props.afspraakId, this.props.icoon.icoonId, coordinates.x, coordinates.y, this.props.aanzicht || null);
                    return false; 
                } else { 
                    this.setState({isOutsideCanvas: false, hasBeenOnCanvas: true});
                    this.props.setPosition(this.props.afspraakId, this.props.icoon.icoonId, coordinates.x, coordinates.y, this.props.aanzicht || null);
                }
            } else if (!this.outsideCanvas(coordinates)) { 
                this.setState({isOutsideCanvas: true, hasBeenOnCanvas: false}); 
                if (this.props.aanzichten) { 
                    const aanzichtenChecked: Map<Aanzicht, Aanzichtpixels> = this.props.aanzichten;
                    const aanzicht: Aanzicht | null = this.calcDropAanzicht(position.x, position.y, aanzichtenChecked);
                    if (aanzicht !== null) {
                        this.props.maakIcoon(this.props.afspraakId, coordinates.x, coordinates.y, this.props.icoon.type, aanzicht);
                    }
                } else { 
                    this.props.maakIcoon(this.props.afspraakId, coordinates.x, coordinates.y, this.props.icoon.type, null);
                }
            } else { 
                this.setState({isOutsideCanvas: true, hasBeenOnCanvas: false}); 
            }
        } else { 
            this.setState({isOutsideCanvas: true, hasBeenOnCanvas: false}); 
        }
    };

    convertPixelsToCoordinates = (position: { x: number, y: number }): Coordinates | null => {
        const icoonAfbeelding: IcoonAfbeelding = getAfbeeldingByType(this.props.icoon.type);
        let aanzicht: Aanzicht | null = null;
        let aanzichtenChecked: Map<Aanzicht, Aanzichtpixels> | null = null;
        if (this.props.aanzichten) { 
            aanzichtenChecked = this.props.aanzichten;
            aanzicht = this.calcDropAanzicht(position.x, position.y, aanzichtenChecked);
        }
        if (this.props.icoon.icoonId) { 
            return this.getConvertedCoordinatesFromPixels(icoonAfbeelding, position, aanzicht);
        } else { 
            if (this.props.aanzichten) { 
                if (aanzicht !== null && aanzichtenChecked) {
                    return {
                        x: convertXPixelsToXCoordinate(position.x - getMandatory(aanzichtenChecked, aanzicht).offsetX, getMandatory(aanzichtenChecked, aanzicht).width,
                            this.icoonWidth()),
                        y: convertYPixelsToYCoordinate((this.props.imageHeight + position.y), getMandatory(aanzichtenChecked, aanzicht).width, this.icoonHeight()),
                        aanzicht: aanzicht,
                    };
                } else { 
                    return null;
                }
            } else { 
                return this.getConvertedCoordinatesFromPixels(icoonAfbeelding, position, null);
            }
        }
    };

    onDrag = (e: Event, position: { x: number, y: number }) => {
        const coordinates: Coordinates | null = this.convertPixelsToCoordinates(position);
        if (coordinates) {
            const draggedIconOutsideCanvas: boolean = this.outsideCanvas(coordinates);
            if (draggedIconOutsideCanvas) {
                this.onStop(e, position);
            }
            this.setState({isOutsideCanvas: draggedIconOutsideCanvas, hasBeenOnCanvas: this.state.hasBeenOnCanvas || !draggedIconOutsideCanvas});
        }
    };

    textChange = (event: Event) => {
        const target = event.target;
        if (target instanceof HTMLInputElement) {
            this.props.verwerkTextChange(this.props.afspraakId, this.props.icoon.icoonId, target.value);
        }
    };

    constructor(props: AnnotatieIcoonProps) {
        super(props);
        this.state = {isOutsideCanvas: false, hasBeenOnCanvas: this.props.icoon.icoonId !== 0};
        this.props = props;
        this.onStop = this.onStop.bind(this);
        this.textboxRef = React.createRef();
    }

    componentDidMount(): * {
        if (this.props.isDraggable && this.props.metTextbox && this.props.icoon.nieuwIcoon) {
            this.textboxRef.current.focus();
        }
    }

    render() {
        const icoon = this.props.icoon;
        const icoonAfbeelding: IcoonAfbeelding = getAfbeeldingByType(icoon.type, this.props.isNietVisueleInspectie);
        return (
            <Draggable position={this.getPixelsFromCoordinates(icoonAfbeelding)} onStop={this.onStop} onDrag={this.onDrag} enableUserSelectHack={false}
                       disabled={!this.props.isDraggable}>
                {this.state.isOutsideCanvas && this.state.hasBeenOnCanvas ? <div/> :
                    <div className={'visuele-inspectie-icoon' + this.notSelectable()} title={annotatieTitle(icoon.type)}
                         style={{
                             backgroundImage: 'url(' + icoonAfbeelding.afbeelding + ')',
                             backgroundRepeat: 'no-repeat',
                             backgroundSize: 'cover',
                             width: this.icoonWidth(),
                             height: this.icoonHeight(),
                         }}>
                        {this.props.metTextbox ?
                            <input ref={this.textboxRef} type="text" disabled={!this.props.isDraggable} defaultValue={icoon.tekst} maxLength="12"
                                   className={'icoontext' + this.notSelectable()} onChange={this.textChange}
                                   style={{
                                       marginTop: this.icoonHeight(),
                                       width: tekstvakbreedte,
                                       marginLeft: (this.icoonWidth() - tekstvakbreedte) / 2,
                                   }}/>
                            :
                            null}
                    </div>}
            </Draggable>
        );
    }

    icoonWidth(): number {
        const icoon = this.props.icoon;
        return icoon.icoonId && icoon.type === 'AMPUTATIE' ? this.props.amputatieSize : getAfbeeldingByType(icoon.type).width;
    }

    icoonHeight(): number {
        const icoon = this.props.icoon;
        return icoon.icoonId && icoon.type === 'AMPUTATIE' ? this.props.amputatieSize : getAfbeeldingByType(icoon.type).height;
    }

    getPixelsFromCoordinates(icoonAfbeelding: IcoonAfbeelding) {
        if (icoonAfbeelding.isRightUpperCornerOrigin === true && this.props.isOpPalet !== true) {
            return {
                x: convertXCoordinateToXPixelsOriginRightUpperCorner(this.props.icoon.positieX, this.props.imageWidth, this.icoonWidth()),
                y: convertYCoordinateToYPixelsOriginRightUpperCorner(this.props.icoon.positieY, this.props.imageWidth) - this.props.imageHeight,
            };
        } else {
            return {
                x: convertXCoordinateToXPixels(this.props.icoon.positieX, this.props.imageWidth, this.icoonWidth()),
                y: convertYCoordinateToYPixels(this.props.icoon.positieY, this.props.imageWidth, this.icoonHeight()) - this.props.imageHeight,
            };
        }
    }

    getConvertedCoordinatesFromPixels(icoonAfbeelding: IcoonAfbeelding, position: { x: number, y: number }, aanzicht: Aanzicht | null): Coordinates | null {
        if (icoonAfbeelding.isRightUpperCornerOrigin === true) {
            return {
                x: convertXPixelsToXCoordinateOriginRightUpperCorner(position.x, this.props.imageWidth, this.icoonWidth()),
                y: convertYPixelsToYCoordinateOriginRightUpperCorner((this.props.imageHeight + position.y), this.props.imageWidth),
                aanzicht: aanzicht,
            };
        } else {
            return {
                x: convertXPixelsToXCoordinate(position.x, this.props.imageWidth, this.icoonWidth()),
                y: convertYPixelsToYCoordinate((this.props.imageHeight + position.y), this.props.imageWidth, this.icoonHeight()),
                aanzicht: aanzicht,
            };
        }
    }

    notSelectable() {
        return (!this.props.isDraggable ? ' no-select' : '');
    }
}
