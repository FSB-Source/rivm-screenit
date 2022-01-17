import React from "react"
import {Table} from "reactstrap"

export type PassantPopupViewProps = {
	naam: string;
	bsn: string;
	geboortedatum: string;
	afspraakVanaf: string;
	afspraakSe?: string;
	uitnodigingsDatum: string;
	eenmaligeAfmelding: boolean;
};

export default class PassantPopupView extends React.Component<PassantPopupViewProps> {

	render(): JSX.Element {
		return <div>
			<div>
				Weet u zeker dat u een afspraak wilt maken voor:
				<Table className={"my-1 table-duo"}>
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

			{this.props.eenmaligeAfmelding ? <div>
				<br/>
				Deze cliënt heeft een eenmalige afmelding.
			</div> : this.props.afspraakVanaf ? <div>
				<br/>
				Deze cliënt heeft al een afspraak in de huidige ronde:
				<Table className={"my-1 table-duo"}>
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
			</div> : this.props.uitnodigingsDatum ? <div>
				<br/>
				Deze cliënt heeft een open uitnodiging:
				<Table className={"my-1 table-duo"}>
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
			</div> : null}
		</div>
	}

}
