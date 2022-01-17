import React, {Component} from "react"
import {Col, Row} from "reactstrap"
import type {VorigOnderzoek} from "../../../datatypes/VorigOnderzoek"
import Paneel from "../../generic/Paneel"
import type {Lezing} from "../../../datatypes/Lezing"
import LezingBlokView from "./LezingBlokView"
import BiradsWaardeView from "./BiradsWaardeView"

export type LezingenBlokViewProps = {
	vorigOnderzoek: VorigOnderzoek;
};

export default class LezingenBlokView extends Component<LezingenBlokViewProps> {

	kolomWidth: number

	constructor(props: LezingenBlokViewProps) {
		super(props)
		this.kolomWidth = 12 / (this.props.vorigOnderzoek.lezingen.length + 1)
	}

	render(): JSX.Element {
		const verslagLezing = this.props.vorigOnderzoek.verslagLezing

		try {
			return <Paneel className={"onderzoek-component"}>
				{this.props.vorigOnderzoek.uitslagGunstig !== null && this.bevatGeenEmptyLezing() && verslagLezing ?
					<div>
						<Row noGutters>
							{this.props.vorigOnderzoek.lezingen.map((lezing: Lezing, index) => {
								return <LezingBlokView key={index}
													   blokId={`${this.props.vorigOnderzoek.uitnodigingsNr}_${index}`}
													   lezing={lezing} colWidth={this.kolomWidth}/>
							})}
							{this.props.vorigOnderzoek.uitslagGunstig ?
								<Col md={this.kolomWidth} className={"px-2 d-flex"}>
									<div className={"d-flex full-width full-width"}>
										<div className={"mt-auto full-width"}>
											<BiradsWaardeView lezing={verslagLezing}/>
										</div>
									</div>
								</Col> : <LezingBlokView blokId={`${this.props.vorigOnderzoek.uitnodigingsNr}_ID`}
														 lezing={verslagLezing} colWidth={this.kolomWidth}/>}
						</Row>
						{this.props.vorigOnderzoek.nevenbevindingen ? <Row noGutters>
							<Col className={"px-2 col-md-4"}>
								<Paneel className={"onderzoek-component"}>
									<Row className={"mb-1"}>
										<span className={"font-weight-bold"}>Gevonden nevenbevindingen: &nbsp;</span>
										{this.props.vorigOnderzoek.nevenbevindingen}
									</Row>
									{this.props.vorigOnderzoek.nevenbevindingenOpmerkingen.length > 0 ? <Row>
										<span className={"font-weight-bold"}>Nevenbevindingen opmerking(en): </span>
										<br/>
										{this.props.vorigOnderzoek.nevenbevindingenOpmerkingen.map((value, key) => {
											return <Row className={"ondezoek-component"} noGutters
														key={key}>{value}</Row>
										})}
									</Row> : null}
								</Paneel>
							</Col>
						</Row> : null}

					</div> : <Row>
						<Col md={12}
							 className={"text-center font-weight-bold"}>{this.props.vorigOnderzoek.onbeoordeelbaar ? "Er was geen beoordeling mogelijk" : "Er is geen afgesloten beoordeling"}</Col>
						<Col md={12}
							 className={"text-center font-weight-bold"}>{this.props.vorigOnderzoek.onbeoordeelbaar ? "Er is geen uitslag omdat er geen beoordeling mogelijk was" : "Er is geen uitslag omdat er geen afgesloten beoordeling is"}</Col>
					</Row>}
			</Paneel>
		} catch (exception: any) {
			console.warn(`fout tijdens aanmaken van lezingen: ${exception.message}`)
			return <Paneel className={"onderzoek-component paneel-shadow"}>Door een technische fout kan de vorige
				onderzoeksinformatie (gedeeltelijk) niet weergegeven worden. U kunt
				het onderzoek normaal verder uitvoeren.</Paneel>
		}
	}

	bevatGeenEmptyLezing = (): boolean => {
		return this.props.vorigOnderzoek.lezingen.filter(lezing => !lezing).length < 1
	}
}