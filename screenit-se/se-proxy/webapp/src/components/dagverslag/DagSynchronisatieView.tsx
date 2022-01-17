import React from "react"
import {Table} from "reactstrap"
import Paneel from "../generic/Paneel"
import type {Dagverslag} from "../../datatypes/Dagverslag"

export type DagSynchronisatieViewStateProps = {
	daglijstDatum: string;
	dagverslag: Map<string, Dagverslag>;
};

export default class DagSynchronisatieView extends React.Component<DagSynchronisatieViewStateProps> {
	render(): JSX.Element {
		const dagverslag = this.props.dagverslag.get(this.props.daglijstDatum)

		if (!dagverslag) {
			return <div/>
		}

		return <Paneel className="dagverslag-paneel">
			<h6>
				Onderzoeken synchronisatie
			</h6>
			<Table className="table table-bordered table-condensed table-hover table-duo">
				<thead>
				<tr>
					<th>
						Onderzoeken gestart
					</th>
					<th>
						Onderzoeken met beelden verwerkt
					</th>
				</tr>
				</thead>
				<tbody>
				<tr>
					<td>
						{dagverslag.dagSynchronisatie.gemaakt}
					</td>
					<td>
						{dagverslag.dagSynchronisatie.verwerkt}
					</td>
				</tr>
				</tbody>
			</Table>
		</Paneel>
	}

}