import React, {Component} from "react"
import type {Lezing} from "../../../datatypes/Lezing"
import {Col, Row} from "reactstrap"
import type {BiradsWaarde} from "../../../datatypes/BiradsWaarde"
import Paneel from "../../generic/Paneel"

export type BiradsWaardeViewProps = {
	lezing: Lezing;
};

export default class BiradsWaardeView extends Component<BiradsWaardeViewProps> {

	static getBiradsBackgroundColor(birads: BiradsWaarde): string {
		return Number(birads) === 0 || Number(birads) === 4 || Number(birads) === 5 ? "background-red" : ""
	}

	render(): JSX.Element {
		try {
			return <Row noGutters className={"px-2"}>
				<Col md={6}>
					<span className={"font-weight-bold"}>{this.props.lezing.lezingType}</span>
				</Col>
				<Col md={6}>
					<span className={"font-weight-bold"}>{this.props.lezing.radioloogNaam}</span>
				</Col>
				<br/>
				<br/>
				<Row noGutters>
					<Col md={12}>
						<span className={"font-weight-bold"}>BI-RADS Beoordeling</span>

					</Col>
					<Col md={2}
						 className={`mr-1 text-center ${BiradsWaardeView.getBiradsBackgroundColor(this.props.lezing.biradsRechts)} border border-gray rounded`}>
						<span>R</span>
						<div>
							<input className={"mr-2"} type={"radio"} disabled={true} checked={true}/>
							<span>{this.props.lezing.biradsRechts}</span>
						</div>
					</Col>
					<Col md={2}
						 className={`ml-1 text-center ${BiradsWaardeView.getBiradsBackgroundColor(this.props.lezing.biradsLinks)} border border-gray rounded`}>
						<span>L</span>
						<div>
							<input className={"mr-2"} type={"radio"} disabled={true} checked={true}/>
							<span>{this.props.lezing.biradsLinks}</span>
						</div>
					</Col>
				</Row>
			</Row>
		} catch (exception: any) {
			console.warn(`fout tijdens aanmaken van BI-RADS waarde: ${exception.message}`)
			return <Paneel className={"onderzoek-component paneel-shadow"}>Door een technische fout kan de BI-RADS
				waarde niet weergegeven worden</Paneel>
		}
	}

}
