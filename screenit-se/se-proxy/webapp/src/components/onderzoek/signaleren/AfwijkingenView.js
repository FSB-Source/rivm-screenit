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
import SignaleringPaletView from './SignaleringPaletView';
import AfwijkingenAfbeeldingenView from './AfwijkingenAfbeeldingenView';
import type {Aanzicht} from '../afbeelding/AnnotatieIcoonContainer';
import type {AnnotatieIcoon, AnnotatieIcoonType} from '../../../datatypes/AnnotatieIcoon';
import AfwijkingenSwitch from '../../generic/AfwijkingenSwitch';
import type {Amputatie} from '../../../datatypes/Onderzoek';

export type AfwijkingenProps = {
    heeftAfwijkingen: boolean,
    magSignaleren: boolean,
    afspraakId: number,
    iconenByIdRechtsVerticaal: Map<number, AnnotatieIcoon>,
    iconenByIdLinksVerticaal: Map<number, AnnotatieIcoon>,
    iconenByIdRechtsHorizontaal: Map<number, AnnotatieIcoon>,
    iconenByIdLinksHorizontaal: Map<number, AnnotatieIcoon>,
    paletIconen: Array<AnnotatieIcoonType>,
    isEditable: boolean,
    amputatie: ?Amputatie,
    afwijkingenToggle: (afspraakId: number, heeftAfwijkingen: boolean) => void,
}

type AfwijkingenState = {
    width: number,
    height: number,
    aanzichten: Map<Aanzicht, { offsetX: number, offsetY: number, width: number, height: number }>,
}

export default class AfwijkingenView extends Component<AfwijkingenProps, AfwijkingenState> {

    updateParentState = () => {
        const element: HTMLElement | null = document ?
            document.getElementById('afwijking-afbeelding-container' + this.getId()) ?
                document.getElementById('afwijking-afbeelding-container' + this.getId()) :
                null :
            null;
        if (element) {
            this.setState({
                width: element.clientWidth,
                height: element.clientHeight,
                aanzichten: AfwijkingenAfbeeldingenView.getRenderedAanzichten(this.getId()),
            });
        }
    };

    constructor(props: AfwijkingenProps) {
        super(props);
        this.props = props;
        this.updateParentState.bind(this);
        this.state = {
            width: 0,
            height: 0,
            aanzichten: new Map(),
        };
    }

    render() {
        return (
            <div className={'signaleren-afwijkingen-container'}>
                {
                    this.props.isEditable ?
                        <AfwijkingenSwitch id={'afwijkingen-button'} heeftAfwijkingen={this.props.heeftAfwijkingen} heeftRecht={this.props.magSignaleren}
                                           rechtNaam={'Signaleren op SE'} placementFeedback={'bottom'} afspraakId={this.props.afspraakId}/>
                        : <p/>
                }

                {
                    this.props.heeftAfwijkingen ?
                        <div id={'afwijking-afbeelding-container' + this.getId()} className={'border border-dark rounded mx-auto'}>
                            <AfwijkingenAfbeeldingenView afspraakId={this.props.afspraakId} afwijkingenAfbeeldingId={this.getId()}
                                                         onLoad={this.updateParentState}
                                                         iconenByIdLinksHorizontaal={this.props.iconenByIdLinksHorizontaal}
                                                         iconenByIdRechtsHorizontaal={this.props.iconenByIdRechtsHorizontaal}
                                                         iconenByIdLinksVerticaal={this.props.iconenByIdLinksVerticaal}
                                                         iconenByIdRechtsVerticaal={this.props.iconenByIdRechtsVerticaal}
                                                         isEditable={this.props.isEditable}
                                                         isNietVisueleInspectie={true}
                                                         amputatie={this.props.amputatie}
                            />
                            {
                                this.props.magSignaleren && this.props.isEditable ?
                                    <SignaleringPaletView afspraakId={this.props.afspraakId} afwijkingenAfbeeldingId={this.getId()}
                                                          paletIconen={this.props.paletIconen} imageWidth={this.state.width}
                                                          imageHeight={this.state.height} aanzichten={this.state.aanzichten}
                                                          isEditable={this.props.isEditable}/>
                                    : null
                            }
                        </div>
                        : null
                }
            </div>
        );
    }

    getId(): string {
        return this.props.afspraakId + '_ID';
    }
};
