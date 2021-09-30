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

import React from 'react';
import {Button, Col, ModalFooter, Row} from 'reactstrap';
import {dispatchActions} from '../../util/DispatchUtil';
import {createActionClearPopup} from '../../actions/PopupActions';
import {store} from '../../Store';
import {showErrorToast} from '../../util/ToastUtil';
import {verstuurAdHocVerzoek} from '../../restclient/AdhocMeekijkverzoekRestClient';

type AdhocKwaliteitscontrolePopupProps = {
    afspraakId: number,
}

type AdhocKwaliteitscontrolePopupState = {
    reden: ?string;
}
const adhocRedenen = [
    'Regulier Onderhoud',
    'Verplaatsing',
    'Storing',
    'Reparatie',
    'Vervanging röntgenbuis',
    'Vervanging detector (LRCB)',
    'Foutieve opname in set',
    'Zichtbare Beeldverstoring',
    'Error tijdens kalibratie',
    'Graag SE bellen ',
    'Op verzoek LRCB',
];

export default class AdhocKwaliteitscontrolePopupView extends React.Component<AdhocKwaliteitscontrolePopupProps, AdhocKwaliteitscontrolePopupState> {

    constructor(props: AdhocKwaliteitscontrolePopupProps) {
        super(props);
        this.props = props;
        this.state = {
            reden: null,
        };
    }

    render() {
        return (
            <div>
                <p>Selecteer een reden en verstuur het verzoek naar het LRCB.</p>
                <Row className={'popupRow'}>
                    {this.maakRedenKeuzeRijen()}
                </Row>
                {this.state.reden ? <p className={'geselecteerde-adhoc-reden-header'}>Gekozen reden:</p> : null}
                <p className={'geselecteerde-adhoc-reden'}>{this.state.reden}</p>
                <ModalFooter>
                    <Button color={this.state.reden ? 'primary' : 'secondary'} onClick={() => {
                        {
                            if (this.state.reden) {
                                this.verstuurVerzoek(this.state.reden, this.props.afspraakId);
                                dispatchActions(store.dispatch, createActionClearPopup());
                            } else {
                                showErrorToast('Selecteer een reden');
                            }
                        }
                    }}>Verzoek versturen</Button>
                    <Button color="secondary" onClick={() => {
                        dispatchActions(store.dispatch, createActionClearPopup());
                    }}>Annuleren</Button>

                </ModalFooter>
            </div>
        );
    }

    maakRedenKeuzeRijen() {
        let result = [];
        adhocRedenen.forEach(reden => {
            result.push(
                <Col key={reden} md={12} className='adhocKeuzeRow' onClick={() => {
                    this.selecteerReden(reden);
                }}>
                    {reden}
                </Col>,
            );
        });
        return result;
    }

    selecteerReden(reden: string): void {
        this.setState({
            reden: reden,
        });
    }

    verstuurVerzoek(reden: string, afspraakId: number): void {
        verstuurAdHocVerzoek(reden, afspraakId);
    }
};
