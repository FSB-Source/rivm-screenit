import React, {Component} from "react"
import type {Huisarts} from "../../../datatypes/Huisarts"
import {Afspraak} from "../../../datatypes/Afspraak"
import paginationFactory from "react-bootstrap-table2-paginator"
import BootstrapTable, {SelectRowProps} from "react-bootstrap-table-next"
import {getMandatory} from "../../../util/MapUtil"
import type {HuisartsZoekFilter} from "./HuisartsZoekenView"

export type HuisartsListItem = {
	id: number;
	naam: string;
	type: string;
	adres: string;
	praktijknaam: string;
};

export type HuisartsLijstStateProps = {
	huisartsen: Map<number, Huisarts>;
	huisartsFilter: HuisartsZoekFilter;
	huisartsItems: Array<HuisartsListItem>;
	afspraak: Afspraak;
	moetVerversen: boolean;
};

export type HuisartsLijstDispatchProps = {
	vergrendelZoekState: () => void;
	onKiesHuisarts: (huisarts: Huisarts) => void;
	toggle: () => void;
}

export type HuisartsLijstState = {
	moetVerversen: boolean;
};

const huisartsTableColumns = [{
	dataField: "naam",
	text: "Naam",
	sort: true,
}, {
	dataField: "type",
	text: "Type",
	sort: true,
	headerClasses: "w-20",
}, {
	dataField: "adres",
	text: "Adres",
	sort: true,
}, {
	dataField: "praktijknaam",
	text: "Praktijknaam",
	sort: true,
}]
const pagination = paginationFactory({
	page: 1,
	sizePerPage: 5,
	alwaysShowAllBtns: true,
	hideSizePerPage: true,
	prePageText: "Vorige",
	nextPageText: "Volgende",
	withFirstAndLast: false,
	showTotal: true,
	paginationTotalRenderer: (from: number, to: number, size: number) => {
		return <span
			className="react-bootstrap-table-pagination-total">
            Totaal: <strong>{size} huisarts(en)</strong>
		</span>
	},
})
export default class HuisartsLijstView extends Component<HuisartsLijstStateProps & HuisartsLijstDispatchProps> {
	selectRow: SelectRowProps<HuisartsListItem> = {
		mode: "checkbox",
		clickToSelect: true,
		hideSelectColumn: true,
		onSelect: (row: HuisartsListItem) => {
			this.props.onKiesHuisarts(getMandatory(this.props.huisartsen, row.id))
			this.props.toggle()
		},
	}

	constructor(props: HuisartsLijstStateProps & HuisartsLijstDispatchProps) {
		super(props)
		this.selectRow.onSelect && this.selectRow.onSelect.bind(this)
	}

	render(): JSX.Element {
		return <div className={"huisarts-table"}>
			<h5 className={"ml-2"}>Er zijn {this.props.huisartsItems.length} huisartsen gevonden</h5>
			{}
			<BootstrapTable
				condensed hover bordered
				rowStyle={{
					cursor: "pointer",
				}}
				pagination={pagination} keyField="id" data={this.props.huisartsItems} columns={huisartsTableColumns} selectRow={this.selectRow}/>
		</div>
	}

	shouldComponentUpdate(props: HuisartsLijstStateProps & HuisartsLijstDispatchProps): boolean {
		this.props.vergrendelZoekState()
		return props.moetVerversen
	}

}