import * as React from "react"
import {Col, Row} from "reactstrap"

export type PaneelNaamProps = {
	titel: string;
	className?: string;
	children?: React.ReactNode;
};

export default class PaneelNaam extends React.Component<PaneelNaamProps> {
	render(): JSX.Element {
		return <Row className={["paneelnaam mb-2", this.props.className].join(" ")}>
			<Col md={10}> {this.props.titel} </Col>
			<Col md={2}> {this.props.children} </Col>
		</Row>
	}
}