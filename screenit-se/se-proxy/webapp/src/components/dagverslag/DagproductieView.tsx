import React from "react"
import {Table} from "reactstrap"
import Paneel from "../generic/Paneel"
import type {Dagverslag} from "../../datatypes/Dagverslag"

export type DagproductieViewProps = {
	daglijstDatum: string;
	dagverslag: Map<string, Dagverslag>;
};

export default class DagproductieView extends React.Component<DagproductieViewProps> {
	render(): JSX.Element {
		const dagverslag = this.props.dagverslag.get(this.props.daglijstDatum)

		if (!dagverslag) {
			return <div/>
		}

		const dagproductieTabelRijen = this.getAlleMedewerkerRijen(dagverslag)
		return <Paneel className="dagverslag-paneel">
			<h6>
				Dagproductie
			</h6>
			<Table className="table table-bordered table-condensed table-hover">
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
	}

	getAlleMedewerkerRijen = (dagverslag?: Dagverslag): JSX.Element[] => {
		const result: JSX.Element[] = []
		if (!dagverslag) {
			return result
		}

		const dagproductie = dagverslag.dagproductie
		for (const medewerker in dagproductie) {
			result.push(<tr key={`${medewerker}-dagproductie`}>
				<td>
					{medewerker}
				</td>
				<td>
					{dagproductie[medewerker]?.ingeschrevenCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.onderzochtCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.afgerondCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.onderbrokenCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.onvolledigCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.afwijkingenCount || 0}
				</td>
			</tr>)
		}

		return result
	}
}
