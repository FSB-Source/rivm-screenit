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
import Paneel from '../generic/Paneel';
import type {Afspraak} from '../../datatypes/Afspraak';
import type {Onderzoek} from '../../datatypes/Onderzoek';
import type {Session} from '../../datatypes/Session';
import type {Dagproductie, Dagverslag} from '../../datatypes/Dagverslag';
import {undefinedOrNull} from '../../util/ValidationUtil';

type DagproductieProps = {
    afspraken: Map<number, Afspraak>,
    onderzoeken: Map<number, Onderzoek>,
    session: Session,
    daglijstDatum: string,
    seGebruikers: Map<number, string>,
    dagverslag: Map<string, Dagverslag>,
}

export default class DagproductieView extends React.Component<DagproductieProps> {

    constructor(props: DagproductieProps) {
        super(props);
        this.props = props;
    }

    render() {
        const dagverslag: ?Dagverslag = this.props.dagverslag.get(this.props.daglijstDatum);
        if (undefinedOrNull(dagverslag)) {
            return (
                <div></div>
            );
        }

        const dagproductieTabelRijen = this.getAlleMedewerkerRijen(dagverslag);
        return (
            <Paneel className='dagverslag-paneel'>
                <h6>
                    Dagproductie
                </h6>
                <Table className='table table-bordered table-condensed table-hover'>
                    <thead>
                    <tr>
                        <th>
                            Medewerker
                        </th>
                        <th>
                            Ingeschreven
                        </th>
                        <th>
                            Onderzocht
                        </th>
                        <th>
                            Afgerond
                        </th>
                        <th>
                            Onderbroken
                        </th>
                        <th>
                            Onvolledig
                        </th>
                        <th>
                            Afwijkingen
                        </th>
                    </tr>
                    </thead>
                    <tbody>
                    {dagproductieTabelRijen}
                    </tbody>
                </Table>
            </Paneel>
        );
    }

    getAlleMedewerkerRijen = (dagverslag: ?Dagverslag) => {
        let result = [];
        if (dagverslag === undefined || dagverslag === null) {
            return result;
        }
        const dagproductie: Dagproductie = dagverslag.dagproductie;
        for (let medewerker in dagproductie) {
            result.push(
                <tr key={medewerker + '-dagproductie'}>
                    <td>
                        {medewerker}
                    </td>
                    <td>
                        {dagproductie[medewerker].ingeschrevenCount === null ? 0 : dagproductie[medewerker].ingeschrevenCount}
                    </td>
                    <td>
                        {dagproductie[medewerker].onderzochtCount === null ? 0 : dagproductie[medewerker].onderzochtCount}
                    </td>
                    <td>
                        {dagproductie[medewerker].afgerondCount === null ? 0 : dagproductie[medewerker].afgerondCount}
                    </td>
                    <td>
                        {dagproductie[medewerker].onderbrokenCount === null ? 0 : dagproductie[medewerker].onderbrokenCount}
                    </td>
                    <td>
                        {dagproductie[medewerker].onvolledigCount === null ? 0 : dagproductie[medewerker].onvolledigCount}
                    </td>
                    <td>
                        {dagproductie[medewerker].afwijkingenCount === null ? 0 : dagproductie[medewerker].afwijkingenCount}
                    </td>
                </tr>,
            );
        }
        return result;
    };
};
