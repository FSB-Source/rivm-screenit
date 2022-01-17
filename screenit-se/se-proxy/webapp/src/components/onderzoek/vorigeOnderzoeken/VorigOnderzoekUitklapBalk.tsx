import React, {Component} from "react"
import {Col, Row} from "reactstrap"

export type VorigOnderzoekUitklapBalkProps = {
	eersteBeeindigdeAfspraakOp: string;
	opgenklapt: boolean;
	uitslagGunstig?: boolean;
	toggleOpengeklapt: () => void;
};

export default class VorigOnderzoekUitklapBalkView extends Component<VorigOnderzoekUitklapBalkProps> {

	constructor(props: VorigOnderzoekUitklapBalkProps) {
		super(props)
		this.props.toggleOpengeklapt.bind(this)
	}

	render(): JSX.Element {
		return <Row noGutters
					className={`${this.props.opgenklapt ? "vorig-onderzoek-header" : "vorig-onderzoek-dichtgeklapt vorig-onderzoek-header"} vorig-onderzoek p-2 px-4 clickable`}
					onClick={this.props.toggleOpengeklapt}>
			<Col sm={1}>
				{this.props.eersteBeeindigdeAfspraakOp ? this.props.eersteBeeindigdeAfspraakOp.substr(0, 4) : ""}
			</Col>
			{this.props.uitslagGunstig === false ? <Col sm={1}>
				<i className="fa fa-exclamation-triangle"/>
			</Col> : null}
		</Row>
	}

}
