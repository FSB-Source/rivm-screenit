import React from "react"
import {Col, Row} from "reactstrap"

export type LabelInputProps = {
	label: string;
	input: JSX.Element;
	mdLabel?: number;
	mdInput?: number;
};

export default class LabelInput extends React.Component<LabelInputProps> {
	render(): JSX.Element {
		return <Row className={"label-input-row"} noGutters={true}>
			<Col md={this.props.mdLabel ? this.props.mdLabel : 6}>{this.props.label}</Col>
			<Col md={this.props.mdInput ? this.props.mdInput : 6}>{this.props.input}</Col>
		</Row>
	}

}