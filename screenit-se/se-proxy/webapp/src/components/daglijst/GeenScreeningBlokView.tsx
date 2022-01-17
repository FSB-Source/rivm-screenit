import React, {Component} from "react"
import {GeenScreeningBlok} from "../../datatypes/Planning"
import {getTime} from "../../util/DateUtil"

type GeenScreeningBlokProps = {
	geenScreeningBlok: GeenScreeningBlok;
};

export default class GeenScreeningBlokView extends Component<GeenScreeningBlokProps> {
	render(): JSX.Element {
		return <tr style={{backgroundColor: "#B9B9B9"}}>
			<td>{`${this.props.geenScreeningBlok.vanafTijd} - ${getTime(this.props.geenScreeningBlok.totDatumTijd)}`}</td>
			<td colSpan={4}>{this.props.geenScreeningBlok.opmerking}</td>
		</tr>
	}

}