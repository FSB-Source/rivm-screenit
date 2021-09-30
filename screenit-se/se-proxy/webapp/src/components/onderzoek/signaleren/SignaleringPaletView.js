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
import {getAfbeeldingByType} from '../../../util/IcoonAfbeeldingTypeUtil';
import AnnotatiePaletView from '../afbeelding/AnnotatiePaletView';
import {Col, Row} from 'reactstrap';
import type {Aanzicht} from '../afbeelding/AnnotatieIcoonContainer';
import AfwijkingenAfbeeldingenView from './AfwijkingenAfbeeldingenView';

export type PaletProps = {
    afspraakId: number,
    afwijkingenAfbeeldingId: string,
    paletIconen: Array<AnnotatieIcoonType>,
    imageWidth: number,
    imageHeight: number,
    isEditable: boolean,
    aanzichten: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }>,
}
const magicYOffset: number = 2;
export default class SignaleringPaletView extends Component<PaletProps> {

    constructor(props: PaletProps) {
        super(props);
        this.props = props;
    }

    getMaxIconHeight() {
        return Math.max(...this.props.paletIconen.map((value: AnnotatieIcoonType) => {
            return getAfbeeldingByType(value).height;
        }));
    }

    render() {
        return (
            <Row noGutters>
                <Col>
                    <AnnotatiePaletView afspraakId={this.props.afspraakId} icoonList={this.props.paletIconen} imageWidth={this.props.imageWidth}
                                        imageHeight={this.props.imageHeight - AfwijkingenAfbeeldingenView.calcYOffset(this.props.afwijkingenAfbeeldingId) + magicYOffset}
                                        paletWidth={this.props.imageWidth / 3}
                                        paletHeight={this.getMaxIconHeight()} aanzichten={this.props.aanzichten}
                                        isEditable={this.props.isEditable}
                    />
                </Col>

            </Row>
        );
    }

}
