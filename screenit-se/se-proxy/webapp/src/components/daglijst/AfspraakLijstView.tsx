import React, {Component} from "react"
import {Afspraak, afspraakHash} from "../../datatypes/Afspraak"
import AfspraakRijContainer from "./AfspraakRijContainer"
import {getMandatory} from "../../util/MapUtil"
import {Table} from "reactstrap"
import type {Tijdslot} from "../../datatypes/Planning"
import {GeenScreeningBlok} from "../../datatypes/Planning"
import GeenScreeningBlokView from "./GeenScreeningBlokView"
import {Client} from "../../datatypes/Client"

export type AfspraakLijstViewProps = {
	afspraken: Array<Tijdslot>;
	clienten: Map<number, Client>;
	emptyText: string;
};

export default class AfspraakLijstView extends Component<AfspraakLijstViewProps> {
	render(): JSX.Element {
		return this.props.afspraken && this.props.afspraken.length > 0 ?
			<Table className="table table-bordered table-condensed table-hover" responsive={true}>
				<thead>
				<tr>
					<th style={{
						width: "10%",
					}}>Tijd
					</th>
					<th style={{
						width: "25%",
					}}>Cliënt
					</th>
					<th style={{
						width: "25%",
					}}>Geboortedatum
					</th>
					<th style={{
						width: "20%",
					}}>BSN
					</th>
					<th style={{
						width: "20%",
					}}>Status
					</th>
				</tr>
				</thead>
				<tbody>
				{this.props.afspraken.map((tijdslot: Tijdslot, index: number) => {
					if (tijdslot instanceof Afspraak) {
						return <AfspraakRijContainer key={afspraakHash(tijdslot)} afspraak={tijdslot}
													 client={getMandatory(this.props.clienten, tijdslot.clientId)}/>
					}

					if (tijdslot instanceof GeenScreeningBlok) {
						return <GeenScreeningBlokView key={tijdslot.vanafDatum + tijdslot.vanafTijd}
													  geenScreeningBlok={tijdslot}/>
					}
					return <div key={index}/>
				})}
				</tbody>
			</Table> : <p>{this.props.emptyText}</p>
	}

}