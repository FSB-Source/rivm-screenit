import React from "react"
import {Col, Row} from "reactstrap"

export type LabelValueProps = {
	label: string;
	value: string;
	mdLabel?: number;
	mdValue?: number;
};

export default class LabelValue extends React.Component<LabelValueProps> {
	render(): JSX.Element {
		return <Row>
			<Col md={this.props.mdLabel && this.props.mdLabel}>{this.props.label}</Col>
			<Col md={this.props.mdValue && this.props.mdValue} className="se-value">{this.props.value}</Col>
		</Row>
	}

}