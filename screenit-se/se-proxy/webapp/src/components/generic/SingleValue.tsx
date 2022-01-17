import React from "react"
import {Col, Row} from "reactstrap"

export type SingleValueProps = {
	value: string;
};

export default class SingleValue extends React.Component<SingleValueProps> {

	render(): JSX.Element {
		return <Row>
			<Col>{this.props.value}</Col>
		</Row>
	}

}