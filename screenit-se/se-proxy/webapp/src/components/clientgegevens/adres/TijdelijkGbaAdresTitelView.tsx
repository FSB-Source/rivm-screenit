import React, {Component} from "react"
import {Col, Row, UncontrolledTooltip} from "reactstrap"

export default class TijdelijkGbaAdresTitelView extends Component<Record<string, any>> {
	render(): JSX.Element {
		return <Col>
			<Row>
				<Col className="paneelnaam">
					Tijdelijk GBA adres
					<i id="AdresTooltip" className="icon-spacing fa fa-info-circle" aria-hidden="true"/>
					<UncontrolledTooltip placement="right" target="AdresTooltip">
						Dit is het correspondentie adres zolang er geen volledig GBA adres is.
					</UncontrolledTooltip>
				</Col>
			</Row>
		</Col>
	}

}