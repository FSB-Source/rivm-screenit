import React from "react"
import {Table} from "reactstrap"
import Paneel from "../generic/Paneel"
import {Dagverslag} from "../../datatypes/Dagverslag"
import {tijdFormaat} from "../../util/DateUtil"

export type DagStatistiekenViewProps = {
	dagverslag?: Dagverslag;
	aantalVerwacht: number;
	aantalIngeschreven: number;
	aantalOnderzoek: number;
	aantalSignaleren: number;
	aantalAfgerond: number;
	aantalOnderbroken: number;
	aantalOnvolledig: number;
	aantalTotaal: number;
};

export default class DagStatistiekenView extends React.Component<DagStatistiekenViewProps> {
	render(): JSX.Element {
		return <Paneel className="dagverslag-paneel">
			<h6>
				Dagstatistieken
			</h6>
			<Table className="table table-bordered table-condensed table-hover table-duo">
				<thead>
				<tr>
					<th>
						Status
					</th>
					<th>
					</th>
				</tr>
				</thead>
				<tbody>
				<tr>
					<td>
						Dagcapaciteit
					</td>
					<td>
						{this.props.dagverslag?.dagPlanningSamenvatting?.dagCapaciteit}
					</td>
				</tr>
				<tr>
					<td>
						Beschikbaar
					</td>
					<td>
						{this.props.dagverslag?.dagPlanningSamenvatting?.beschikbaarheid}
					</td>
				</tr>
				<tr>
					<td>
						Starttijd
					</td>
					<td>
						{this.props.dagverslag?.dagPlanningSamenvatting && tijdFormaat(this.props.dagverslag.dagPlanningSamenvatting.starttijd)}
					</td>
				</tr>
				<tr className="dagverslag-paneel-scheiding">
					<td>
						Eindtijd
					</td>
					<td>
						{this.props.dagverslag?.dagPlanningSamenvatting && tijdFormaat(this.props.dagverslag.dagPlanningSamenvatting.eindtijd)}
					</td>
				</tr>
				<tr>
					<td>
						Verwacht
					</td>
					<td>
						<span>{this.props.aantalVerwacht}</span>

					</td>
				</tr>
				<tr>
					<td>
						Ingeschreven
					</td>
					<td>
						<span>{this.props.aantalIngeschreven}</span>
					</td>
				</tr>
				<tr>
					<td>
						Onderzoek
					</td>
					<td>
						<span>{this.props.aantalOnderzoek}</span>
					</td>
				</tr>
				<tr>
					<td>
						Signaleren
					</td>
					<td>
						<span>{this.props.aantalSignaleren}</span>
					</td>
				</tr>
				<tr>
					<td>
						Afgerond
					</td>
					<td>
						<span>{this.props.aantalAfgerond}</span>
					</td>
				</tr>
				<tr>
					<td>
						Onderbroken
					</td>
					<td>
						<span>{this.props.aantalOnderbroken}</span>
					</td>
				</tr>
				<tr>
					<td>
						Onvolledig
					</td>
					<td>
						<span>{this.props.aantalOnvolledig}</span>
					</td>
				</tr>
				<tr>
					<th>
						Totaal
					</th>
					<th>
						<span>{this.props.aantalTotaal}</span>
					</th>
				</tr>
				</tbody>
			</Table>
		</Paneel>
	}

}