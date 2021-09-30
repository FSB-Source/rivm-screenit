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
import type {Huisarts} from '../../../datatypes/Huisarts';
import {Afspraak} from '../../../datatypes/Afspraak';
import paginationFactory from 'react-bootstrap-table2-paginator';
import BootstrapTable from 'react-bootstrap-table-next';
import {getMandatory} from '../../../util/MapUtil';
import type {HuisartsZoekFilter} from './HuisartsZoekenView';

export type HuisartsListItem = {
    id: number,
    naam: string,
    type: string,
    adres: string,
    praktijknaam: string
}

export type HuisartsLijstProps = {
    huisartsen: Map<number, Huisarts>,
    huisartsFilter: HuisartsZoekFilter,
    huisartsItems: Array<HuisartsListItem>;
    afspraak: Afspraak;
    moetVerversen: boolean;
    vergrendelZoekState: () => void;

    onKiesHuisarts: (afspraak: Afspraak, huisarts: Huisarts) => void;
    toggle: () => void,
}

export type HuisartsLijstState = {
    moetVerversen: boolean;
}

const huisartsTableColumns = [
    {
        dataField: 'naam',
        text: 'Naam',
        sort: true,
    }, {
        dataField: 'type',
        text: 'Type',
        sort: true,
        headerClasses: 'w-20',
    }, {
        dataField: 'adres',
        text: 'Adres',
        sort: true,
    }
    , {
        dataField: 'praktijknaam',
        text: 'Praktijknaam',
        sort: true,
    }];

const pagination = paginationFactory({
    page: 1,
    sizePerPage: 5,
    alwaysShowAllBtns: true,
    hideSizePerPage: true,
    prePageText: 'Vorige',
    nextPageText: 'Volgende',
    withFirstAndLast: false,
    showTotal: true,
    paginationTotalRenderer: (from: number, to: number, size: number) => (
        <span className="react-bootstrap-table-pagination-total">
            Totaal: <strong>{size} huisarts(en)</strong>
        </span>
    ),
});

export default class HuisartsLijstView extends Component<HuisartsLijstProps> {

    constructor(props: HuisartsLijstProps) {
        super(props);
        this.props = props;
        this.selectRow.onSelect.bind(this);
    };

    selectRow = {
        mode: 'checkbox',
        clickToSelect: true,
        hideSelectColumn: true,
        onSelect: (row: HuisartsListItem, isSelect: boolean, rowIndex: number, e: any) => {
            this.props.onKiesHuisarts(this.props.afspraak, getMandatory(this.props.huisartsen, row.id));
            this.props.toggle();
        },
    };

    render() {
        return (
            <div className={'huisarts-table'}>
                <h5 className={'ml-2'}>Er zijn {this.props.huisartsItems.length} huisartsen gevonden</h5>
                <BootstrapTable condensed hover bordered rowStyle={{cursor: 'pointer'}} pagination={pagination} keyField='id'
                                data={this.props.huisartsItems}
                                columns={huisartsTableColumns}
                                selectRow={this.selectRow}/></div>);
    }

    shouldComponentUpdate(props: HuisartsLijstProps) {
        this.props.vergrendelZoekState();
        return this.props.moetVerversen;
    }
}
