import React, {Component} from "react"
import {Col} from "reactstrap"
import {postcodeMetSpatie} from "../../../datatypes/Adres"
import SingleValue from "../../generic/SingleValue"
import type {TijdelijkAdres} from "../../../datatypes/TijdelijkAdres"
import {getLocatie} from "../../../datatypes/TijdelijkAdres"
import {datumFormaat} from "../../../util/DateUtil"

type TijdelijkAdresViewProps = {
	tijdelijkAdres: TijdelijkAdres;
	disabled: boolean;
	className?: string;
};

export default class TijdelijkAdresView extends Component<TijdelijkAdresViewProps> {
	render(): JSX.Element {
		const tijdelijkAdres: TijdelijkAdres = this.props.tijdelijkAdres
		return <Col className={this.props.className}>
			<SingleValue value={getLocatie(tijdelijkAdres)}/>
			<SingleValue value={`${postcodeMetSpatie(tijdelijkAdres.postcode)} ${tijdelijkAdres.plaats || ""}`}/>
			{tijdelijkAdres.startDatum || tijdelijkAdres.eindDatum ? <SingleValue
				value={`${datumFormaat(tijdelijkAdres.startDatum)}
				 t/m ${datumFormaat(tijdelijkAdres.eindDatum)}`}/> : null}
		</Col>
	}
}