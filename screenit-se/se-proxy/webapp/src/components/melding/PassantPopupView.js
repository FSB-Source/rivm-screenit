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
import {Table} from 'reactstrap';

type PassantPopupProps = {
    naam: string,
    bsn: string,
    geboortedatum: string,
    afspraakVanaf: string,
    afspraakSe: string,
    uitnodigingsDatum: string,
    eenmaligeAfmelding: boolean
}

export default class PassantPopupView extends React.Component<PassantPopupProps> {

    constructor(props: PassantPopupProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <div>
                <div>
                    Weet u zeker dat u een afspraak wilt maken voor:
                    <Table className={'my-1 table-duo'}>
                        <tbody>
                        <tr>
                            <td>
                                Naam
                            </td>
                            <td>
                                {this.props.naam}
                            </td>
                        </tr>
                        <tr>
                            <td>
                                BSN
                            </td>
                            <td>
                                {this.props.bsn}
                            </td>
                        </tr>
                        <tr>
                            <td>
                                Geboortedatum
                            </td>
                            <td>
                                {this.props.geboortedatum}
                            </td>
                        </tr>

                        </tbody>
                    </Table>
                </div>

                {this.props.eenmaligeAfmelding ?
                    <div>
                        <br/>
                        Deze cliënt heeft een eenmalige afmelding.
                    </div>
                    : this.props.afspraakVanaf ?
                        <div>
                            <br/>
                            Deze cliënt heeft al een afspraak in de huidige ronde:
                            <Table className={'my-1 table-duo'}>
                                <tbody>
                                <tr>
                                    <td>
                                        Afspraakdatum
                                    </td>
                                    <td>
                                        {this.props.afspraakVanaf}
                                    </td>
                                </tr>
                                <tr>
                                    <td>
                                        Screeningseenheid
                                    </td>
                                    <td>
                                        {this.props.afspraakSe}
                                    </td>
                                </tr>
                                </tbody>
                            </Table>
                        </div>
                        : this.props.uitnodigingsDatum ?
                            <div>
                                <br/>
                                Deze cliënt heeft een open uitnodiging:
                                <Table className={'my-1 table-duo'}>
                                    <tbody>
                                    <tr>
                                        <td>
                                            Briefdatum
                                        </td>
                                        <td>
                                            {this.props.uitnodigingsDatum}
                                        </td>
                                    </tr>
                                    </tbody>
                                </Table>
                            </div>
                            : null}
            </div>
        );
    }

};
