import React from "react"
import {Container, Row} from "reactstrap"
import type {ErrorDto} from "../../datatypes/ErrorDto"

type ErrorProps = {
	error: ErrorDto;
};

export default class ErrorView extends React.Component<ErrorProps> {
	render(): JSX.Element {
		return <Container fluid>
			<Row>
				<h2 className={"paneelnaam"}>Foutmelding</h2>
			</Row>
			<Row>
				<p>
					Er is een fout opgetreden in de applicatie met foutcode {this.props.error.errorReferentie}. Graag
					alleen deze code gebruiken tijdens correspondentie over deze fout.
				</p>
			</Row>
			<Row>
				<p>
					Gelieve geen screenshot, maar alleen de foutcode doorgeven.
				</p>
			</Row>
		</Container>
	}

}
