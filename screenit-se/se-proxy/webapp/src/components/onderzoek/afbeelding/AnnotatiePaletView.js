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
import type {AnnotatieIcoonType} from '../../../datatypes/AnnotatieIcoon';
import {convertXPixelsToXCoordinate, convertYPixelsToYCoordinate} from '../../../util/CoordinatenCalculator';
import type {Aanzicht} from './AnnotatieIcoonContainer';
import AnnotatieIcoonContainer from './AnnotatieIcoonContainer';
import {getAfbeeldingByType} from '../../../util/IcoonAfbeeldingTypeUtil';

export type AnnotatiePaletProps = {
    afspraakId: number,
    icoonList: Array<AnnotatieIcoonType>,
    imageWidth: number,
    imageHeight: number,
    paletWidth?: number,
    paletHeight: number,
    isEditable: ?boolean,
    aanzichten?: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }>,
}

const yPadding: number = 5;

export default class AnnotatiePaletView extends Component<AnnotatiePaletProps> {

    constructor(props: AnnotatiePaletProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <div style={{height: this.props.paletHeight + yPadding + 'px', paddingBottom: yPadding + 'px'}}>
                {
                    this.props.icoonList.map((icoonType: AnnotatieIcoonType) => {
                        return <AnnotatieIcoonContainer {...this.props} key={icoonType} isOpPalet={true} isDraggable={this.props.isEditable}
                                                        icoon={{
                                                            icoonId: 0,
                                                            positieX: this.getScaledX(this.props.icoonList, icoonType),
                                                            positieY: this.getScaledYOffset(icoonType),
                                                            type: icoonType,
                                                        }}/>;
                    })
                }
            </div>);
    }

    getScaledX(iconen: Array<AnnotatieIcoonType>, icoon: AnnotatieIcoonType) {
        const icoonWidth: number = getAfbeeldingByType(icoon).width;
        const spacing: number = (this.props.paletWidth ? this.props.paletWidth : this.props.imageWidth) / iconen.length;
        const spacingMargin: number = spacing / iconen.length;
        const pixelWidth = iconen.slice(0, iconen.indexOf(icoon)).reduce((accumulator) => accumulator + spacing, spacingMargin);
        return convertXPixelsToXCoordinate(pixelWidth + (this.props.paletWidth ? this.props.paletWidth : 0), this.props.imageWidth, spacing - (icoonWidth / 2));
    }

    getScaledYOffset(icoon: AnnotatieIcoonType) {
        const icoonHeight = getAfbeeldingByType(icoon).height;
        return convertYPixelsToYCoordinate(yPadding * 2 + this.props.imageHeight, this.props.imageWidth, this.props.paletHeight - (icoonHeight / 2));
    }

}
