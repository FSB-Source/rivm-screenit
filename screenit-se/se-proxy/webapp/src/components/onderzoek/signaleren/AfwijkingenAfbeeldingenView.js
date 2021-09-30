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
import RechterborstVerticaleDoorsnede from './beoordeling/RechterborstVerticaleDoorsnede.svg';
import LinkerborstVerticaleDoorsnede from './beoordeling/LinkerborstVerticaleDoorsnede.svg';
import RechterborstHorizontaleDoorsnede from './beoordeling/RechterborstHorizontaleDoorsnede.svg';
import LinkerborstHorizontaleDoorsnede from './beoordeling/LinkerborstHorizontaleDoorsnede.svg';
import {Col, Row} from 'reactstrap';
import type {Aanzicht} from '../afbeelding/AnnotatieIcoonContainer';
import AnnotatieIcoonContainer from '../afbeelding/AnnotatieIcoonContainer';
import type {AnnotatieIcoon} from '../../../datatypes/AnnotatieIcoon';
import * as CoordinationCalculator from '../../../util/CoordinatenCalculator';
import {getAfbeeldingByType} from '../../../util/IcoonAfbeeldingTypeUtil';
import type {Amputatie} from '../../../datatypes/Onderzoek';

export type AfwijkingenAfbeeldingenProps = {
    afspraakId?: number,
    afwijkingenAfbeeldingId: string,
    iconenByIdRechtsVerticaal: Map<number, AnnotatieIcoon>,
    iconenByIdLinksVerticaal: Map<number, AnnotatieIcoon>,
    iconenByIdRechtsHorizontaal: Map<number, AnnotatieIcoon>,
    iconenByIdLinksHorizontaal: Map<number, AnnotatieIcoon>,
    isEditable?: boolean,
    isNietVisueleInspectie?: boolean,
    onLoad: () => void,
    amputatie: ?Amputatie,
}

type AfwijkingenAfbeeldingenState = {
    aanzichten: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }>
}

export default class AfwijkingenAfbeeldingenView extends Component<AfwijkingenAfbeeldingenProps, AfwijkingenAfbeeldingenState> {

    constructor(props: AfwijkingenAfbeeldingenProps) {
        super(props);
        this.props = props;
        this.state = {
            aanzichten: new Map(),
        };
    }

    static calcRechtsVerticaalOffset(afwijkingenAfbeeldingId: string) {
        const element: HTMLElement | null = document && document.getElementById('verticaal_aanzicht_container' + afwijkingenAfbeeldingId) ?
            document.getElementById('verticaal_aanzicht_container' + afwijkingenAfbeeldingId) :
            null;
        if (element) {
            return parseFloat(window.getComputedStyle(element).paddingLeft);
        }
        return 0;
    };

    static calcLinksVerticaalOffset(afwijkingenAfbeeldingId: string): number {
        const elementLeft: HTMLElement | null = document && document.getElementById('img_container_rechts_verticaal' + afwijkingenAfbeeldingId) ?
            document.getElementById('img_container_rechts_verticaal' + afwijkingenAfbeeldingId) : null;
        const element: HTMLElement | null = document && document.getElementById('img_container_links_verticaal' + afwijkingenAfbeeldingId) ?
            document.getElementById('img_container_links_verticaal' + afwijkingenAfbeeldingId) : null;
        if (elementLeft && element) {
            return this.calcRechtsVerticaalOffset(afwijkingenAfbeeldingId) + elementLeft.clientWidth + parseFloat(window.getComputedStyle(element).paddingLeft);
        }
        return 0;
    }

    static calcRechtsHorizontaalOffset(afwijkingenAfbeeldingId: string): number {
        const element: HTMLElement | null = document && document.getElementById('verticaal_aanzicht_container' + afwijkingenAfbeeldingId) ?
            document.getElementById('verticaal_aanzicht_container' + afwijkingenAfbeeldingId) :
            null;
        const elementContainer: HTMLElement | null = document && document.getElementById('horizontaal_aanzicht_container' + afwijkingenAfbeeldingId) ?
            document.getElementById('horizontaal_aanzicht_container' + afwijkingenAfbeeldingId) :
            null;
        const ownElement: HTMLElement | null = document && document.getElementById('img_container_rechts_horizontaal' + afwijkingenAfbeeldingId) ?
            document.getElementById('img_container_rechts_horizontaal' + afwijkingenAfbeeldingId) : null;
        if (element && elementContainer) {
            return this.calcRechtsVerticaalOffset(afwijkingenAfbeeldingId) + element.clientWidth + parseFloat(window.getComputedStyle(ownElement).paddingLeft);
        }
        return 0;
    }

    static calcLinksHorizontaalOffset(afwijkingenAfbeeldingId: string): number {
        const element: HTMLElement | null = document && document.getElementById('img_container_rechts_horizontaal' + afwijkingenAfbeeldingId) ?
            document.getElementById('img_container_rechts_horizontaal' + afwijkingenAfbeeldingId) : null;
        const ownElement: HTMLElement | null = document && document.getElementById('img_container_holder_links_horizontaal' + afwijkingenAfbeeldingId) ?
            document.getElementById('img_container_holder_links_horizontaal' + afwijkingenAfbeeldingId) : null;
        if (element) {
            return (this.calcRechtsHorizontaalOffset(afwijkingenAfbeeldingId) + (element.clientWidth - parseFloat(window.getComputedStyle(element).paddingLeft)) +
                parseFloat(window.getComputedStyle(element).marginRight) + parseFloat(window.getComputedStyle(ownElement).marginLeft));
        }
        return 0;
    }

    static calcYOffset(afwijkingenAfbeeldingId: string): number {
        const elementMediaal: HTMLElement | null = document && document.getElementById('mediaal-text' + afwijkingenAfbeeldingId) ?
            document.getElementById('mediaal-text' + afwijkingenAfbeeldingId) :
            null;

        const elementLateraal: HTMLElement | null = document && document.getElementById('lateraal-text' + afwijkingenAfbeeldingId) ?
            document.getElementById('lateraal-text' + afwijkingenAfbeeldingId) :
            null;
        if (elementMediaal && elementLateraal) {
            return parseFloat(window.getComputedStyle(elementMediaal).marginBottom) + elementLateraal.clientHeight +
                parseFloat(window.getComputedStyle(elementLateraal).marginBottom) +
                elementMediaal.clientHeight;
        }
        return 0;
    }

    static getRenderedAanzichten(afwijkingenAfbeeldingId: string): Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }> {
        const aanzichtenMap: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }> = new Map();

        const elementRechtsVerticaal: HTMLElement | null = document ?
            document.getElementById('rechts_verticaal' + afwijkingenAfbeeldingId) ?
                document.getElementById('rechts_verticaal' + afwijkingenAfbeeldingId) :
                null :
            null;
        aanzichtenMap.set('RECHTS_VERTICAAL', {
            offsetX: this.calcRechtsVerticaalOffset(afwijkingenAfbeeldingId),
            offsetY: this.calcYOffset(afwijkingenAfbeeldingId),
            width: elementRechtsVerticaal ? elementRechtsVerticaal.clientWidth : 0,
            height: elementRechtsVerticaal ? elementRechtsVerticaal.clientHeight : 0,
        });

        const elementLinksVerticaal: HTMLElement | null = document ?
            document.getElementById('links_verticaal' + afwijkingenAfbeeldingId) ?
                document.getElementById('links_verticaal' + afwijkingenAfbeeldingId) :
                null :
            null;
        aanzichtenMap.set('LINKS_VERTICAAL', {
            offsetX: this.calcLinksVerticaalOffset(afwijkingenAfbeeldingId),
            offsetY: this.calcYOffset(afwijkingenAfbeeldingId),
            width: elementLinksVerticaal ? elementLinksVerticaal.clientWidth : 0,
            height: elementLinksVerticaal ? elementLinksVerticaal.clientHeight : 0,
        });

        const elementRechtsHorizontaal: HTMLElement | null = document ?
            document.getElementById('rechts_horizontaal' + afwijkingenAfbeeldingId) ?
                document.getElementById('rechts_horizontaal' + afwijkingenAfbeeldingId) :
                null :
            null;
        aanzichtenMap.set('RECHTS_HORIZONTAAL', {
            offsetX: this.calcRechtsHorizontaalOffset(afwijkingenAfbeeldingId),
            offsetY: this.calcYOffset(afwijkingenAfbeeldingId),
            width: elementRechtsHorizontaal ? elementRechtsHorizontaal.clientWidth : 0,
            height: elementRechtsHorizontaal ? elementRechtsHorizontaal.clientHeight : 0,
        });

        const elementLinksHorizontaal: HTMLElement | null = document ?
            document.getElementById('links_horizontaal' + afwijkingenAfbeeldingId) ?
                document.getElementById('links_horizontaal' + afwijkingenAfbeeldingId) :
                null :
            null;
        aanzichtenMap.set('LINKS_HORIZONTAAL', {
            offsetX: this.calcLinksHorizontaalOffset(afwijkingenAfbeeldingId),
            offsetY: this.calcYOffset(afwijkingenAfbeeldingId),
            width: elementLinksHorizontaal ? elementLinksHorizontaal.clientWidth : 0,
            height: elementLinksHorizontaal ? elementLinksHorizontaal.clientHeight : 0,
        });
        return aanzichtenMap;
    };

    render() {
        return (
            <Row noGutters id={'col-afwijking-afbeelding-container' + this.props.afwijkingenAfbeeldingId}>
                <Col md={6} id={'verticaal_aanzicht_container' + this.props.afwijkingenAfbeeldingId} className={'form-horizontal px-1'}>
                    <p className="text-center non-selectable text-dark">craniaal</p>
                    <Row noGutters className={'afwijking-verticaal_rij'}>
                        <Col md={6} className={'form-horizontal'}>
                            <div id={'img_container_rechts_verticaal' + this.props.afwijkingenAfbeeldingId} className="pr-1 non-selectable">
                                <img id={'rechts_verticaal' + this.props.afwijkingenAfbeeldingId} className={'aanzicht-afbeelding'} src={RechterborstVerticaleDoorsnede}
                                     draggable="false" alt="" onLoad={() => {
                                    this.props.onLoad();
                                }}/>
                                {
                                    this.props.amputatie === 'RECHTERBORST' ? this.amputatieIcoon() :
                                        this.props.iconenByIdRechtsVerticaal ?
                                            Array.from(this.props.iconenByIdRechtsVerticaal.values()).map((icoon: AnnotatieIcoon) => {
                                                return <AnnotatieIcoonContainer key={icoon.icoonId}
                                                                                afspraakId={this.props.afspraakId} icoon={icoon}
                                                                                imageWidth={CoordinationCalculator.getWidth(
                                                                                    'rechts_verticaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                imageHeight={CoordinationCalculator.getHeight(
                                                                                    'rechts_verticaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                aanzicht={'RECHTS_VERTICAAL'} isDraggable={this.props.isEditable}
                                                />;
                                            }) :
                                            null
                                }
                            </div>
                        </Col>
                        <Col md={6} className={'form-horizontal'}>
                            <div id={'img_container_links_verticaal' + this.props.afwijkingenAfbeeldingId} className="pl-1 non-selectable">
                                <img id={'links_verticaal' + this.props.afwijkingenAfbeeldingId} className={'aanzicht-afbeelding'} src={LinkerborstVerticaleDoorsnede}
                                     draggable="false" alt="" onLoad={() => {
                                    this.props.onLoad();
                                }}/>
                                {
                                    this.props.amputatie === 'LINKERBORST' ? this.amputatieIcoon() :
                                        this.props.iconenByIdLinksVerticaal ?
                                            Array.from(this.props.iconenByIdLinksVerticaal.values()).map((icoon: AnnotatieIcoon) => {
                                                return <AnnotatieIcoonContainer key={icoon.icoonId} afspraakId={this.props.afspraakId} icoon={icoon}
                                                                                imageWidth={CoordinationCalculator.getWidth('links_verticaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                imageHeight={CoordinationCalculator.getHeight(
                                                                                    'links_verticaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                aanzicht={'LINKS_VERTICAAL'} isDraggable={this.props.isEditable}
                                                />;
                                            }) :
                                            null
                                }
                            </div>
                        </Col>
                    </Row>
                    <p className="text-center non-selectable text-dark">caudaal</p>
                </Col>
                <Col md={6} id={'horizontaal_aanzicht_container' + this.props.afwijkingenAfbeeldingId} className={'form-horizontal px-1'}>
                    <p id={'lateraal-text' + this.props.afwijkingenAfbeeldingId} className="text-center non-selectable text-dark">lateraal</p>
                    <Row noGutters>
                        <Col md={6} className={'form-horizontal'}>
                            <div id={'img_container_rechts_horizontaal' + this.props.afwijkingenAfbeeldingId}
                                 className="margin-right-correction padding-left-correction non-selectable">
                                <img id={'rechts_horizontaal' + this.props.afwijkingenAfbeeldingId} className={'aanzicht-afbeelding'} src={RechterborstHorizontaleDoorsnede}
                                     draggable="false" alt="" onLoad={() => {
                                    this.props.onLoad();
                                }}/>
                                {
                                    this.props.amputatie === 'RECHTERBORST' ? this.amputatieIcoon() :
                                        this.props.iconenByIdRechtsHorizontaal ?
                                            Array.from(this.props.iconenByIdRechtsHorizontaal.values()).map((icoon: AnnotatieIcoon) => {
                                                return <AnnotatieIcoonContainer key={icoon.icoonId} afspraakId={this.props.afspraakId} icoon={icoon}
                                                                                imageWidth={CoordinationCalculator.getWidth(
                                                                                    'rechts_horizontaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                imageHeight={CoordinationCalculator.getHeight('rechts_horizontaal' +
                                                                                    this.props.afwijkingenAfbeeldingId)}
                                                                                aanzicht={'RECHTS_HORIZONTAAL'} isDraggable={this.props.isEditable}
                                                />;
                                            }) :
                                            null
                                }
                            </div>
                        </Col>
                        <Col md={6} id={'img_container_links_horizontaal' + this.props.afwijkingenAfbeeldingId} className={'form-horizontal'}>
                            <div id={'img_container_holder_links_horizontaal' + this.props.afwijkingenAfbeeldingId}
                                 className="margin-left-correction padding-right-correction  non-selectable">
                                <img id={'links_horizontaal' + this.props.afwijkingenAfbeeldingId} className={'aanzicht-afbeelding'} src={LinkerborstHorizontaleDoorsnede}
                                     draggable="false" alt="" onLoad={() => {
                                    this.props.onLoad();
                                }}/>
                                {
                                    this.props.amputatie === 'LINKERBORST' ? this.amputatieIcoon() :
                                        this.props.iconenByIdLinksHorizontaal ?
                                            Array.from(this.props.iconenByIdLinksHorizontaal.values()).map((icoon: AnnotatieIcoon) => {
                                                return <AnnotatieIcoonContainer key={icoon.icoonId} afspraakId={this.props.afspraakId} icoon={icoon}
                                                                                imageWidth={CoordinationCalculator.getWidth(
                                                                                    'links_horizontaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                imageHeight={CoordinationCalculator.getHeight(
                                                                                    'links_horizontaal' + this.props.afwijkingenAfbeeldingId)}
                                                                                aanzicht={'LINKS_HORIZONTAAL'} isDraggable={this.props.isEditable}
                                                />;
                                            }) :
                                            null
                                }
                            </div>
                        </Col>
                    </Row>
                    <p id={'mediaal-text' + this.props.afwijkingenAfbeeldingId} className="text-center non-selectable text-dark">mediaal</p>
                </Col>
            </Row>

        );
    }

    amputatieIcoon() {
        return <img src={getAfbeeldingByType('AMPUTATIE', this.props.isNietVisueleInspectie).afbeelding} width={'50%'} height={'50%'}
                    style={{position: 'absolute', top: '35%', left: '25%'}}
                    alt={'X'}/>;
    }
};
