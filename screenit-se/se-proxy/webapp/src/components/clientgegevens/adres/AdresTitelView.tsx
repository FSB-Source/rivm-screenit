import classNames from "classnames"
import React, {Component} from "react"
import {Col} from "reactstrap"

type AdresTitelViewProps = {
	adresTitel: string;
	className?: string;
};

export default class AdresTitelView extends Component<AdresTitelViewProps> {
	render(): JSX.Element {
		return <Col>
			<div className={classNames(this.props.className, "paneelnaam")}>
				{this.props.adresTitel}
			</div>
		</Col>
	}

}