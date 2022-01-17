import React from "react"
import {Table} from "reactstrap"
import Paneel from "../generic/Paneel"
import type {Dagverslag} from "../../datatypes/Dagverslag"

export type DagAfrondstatusViewStateProps = {
	daglijstDatum: string;
	totaalAfgerond: number;
	dagverslag: Map<string, Dagverslag>;
};

export default class DagAfrondstatusView extends React.Component<DagAfrondstatusViewStateProps> {
	render(): JSX.Element {
		const dagverslag = this.props.dagverslag.get(this.props.daglijstDatum)
		if (!dagverslag) {
			return <div/>
		}

		return <Paneel className="dagverslag-paneel">
			<h6>
				Onderzoeken doorgevoerd
			</h6>
			<Table className="table table-bordered table-condensed table-hover table-duo">
				<thead>
				<tr>
					<th>
						Afgerond/Onderbroken/Onvolledig
					</th>
					<th>
						Doorgevoerd
					</th>
				</tr>
				</thead>
				<tbody>
				<tr>
					<td>
						{this.props.totaalAfgerond}
					</td>
					<td>
						{dagverslag.dagafsluiting.aantalDoorgevoerd}
					</td>
				</tr>
				</tbody>
			</Table>
		</Paneel>
	}

}