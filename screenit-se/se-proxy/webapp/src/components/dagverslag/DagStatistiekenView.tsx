import React from "react"
import {Table} from "reactstrap"
import Paneel from "../generic/Paneel"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Onderzoek} from "../../datatypes/Onderzoek"
import {getOnderzoekStatusCount} from "../../util/AfsprakenUtil"
import {Dagverslag} from "../../datatypes/Dagverslag"
import {tijdFormaat} from "../../util/DateUtil"

export type DagStatistiekenViewProps = {
	afspraken: Afspraak[];
	onderzoeken: Map<number, Onderzoek>;
	afsprakenLength: number;
	dagverslag?: Dagverslag;
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
						{this.props.dagverslag && this.props.dagverslag.dagPlanningSamenvatting.dagCapaciteit}
					</td>
				</tr>
				<tr>
					<td>
						Beschikbaar
					</td>
					<td>
						{this.props.dagverslag && this.props.dagverslag.dagPlanningSamenvatting.beschikbaarheid}
					</td>
				</tr>
				<tr>
					<td>
						Starttijd
					</td>
					<td>
						{this.props.dagverslag && tijdFormaat(this.props.dagverslag.dagPlanningSamenvatting.starttijd)}
					</td>
				</tr>
				<tr className="dagverslag-paneel-scheiding">
					<td>
						Eindtijd
					</td>
					<td>
						{this.props.dagverslag && tijdFormaat(this.props.dagverslag.dagPlanningSamenvatting.eindtijd)}
					</td>
				</tr>
				<tr>
					<td>
						Verwacht
					</td>
					<td>
						{getOnderzoekStatusCount("VERWACHT", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<td>
						Ingeschreven
					</td>
					<td>
						{getOnderzoekStatusCount("INGESCHREVEN", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<td>
						Onderzoek
					</td>
					<td>
						{getOnderzoekStatusCount("ONDERZOEK", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<td>
						Signaleren
					</td>
					<td>
						{getOnderzoekStatusCount("SIGNALEREN", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<td>
						Afgerond
					</td>
					<td>
						{getOnderzoekStatusCount("AFGEROND", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<td>
						Onderbroken
					</td>
					<td>
						{getOnderzoekStatusCount("ONDERBROKEN", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<td>
						Onvolledig
					</td>
					<td>
						{getOnderzoekStatusCount("ONVOLLEDIG", this.props.onderzoeken, this.props.afspraken)}
					</td>
				</tr>
				<tr>
					<th>
						Totaal
					</th>
					<th>
						{this.props.afsprakenLength}
					</th>
				</tr>
				</tbody>
			</Table>
		</Paneel>
	}

}