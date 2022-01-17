import React, {Component} from "react"
import Paneel from "../generic/Paneel"
import PaneelNaam from "../generic/PaneelNaam"
import CheckboxValue from "../generic/CheckboxValue"

export type BezwaarStateProps = {
	afspraakId: number;
	bezwaarAangevraagd: boolean;
	isIngeschreven: boolean;
	bezwaarDoorgevoerdOpCentraal: boolean;
};

export type BezwaarDispatchProps = {
	onBezwaarAangevraagd: (afspraakId: number, bezwaar: boolean, isIngeschreven: boolean) => void;
}

export default class BezwaarView extends Component<BezwaarStateProps & BezwaarDispatchProps> {

	constructor(props: BezwaarStateProps & BezwaarDispatchProps) {
		super(props)
		this.bezwaarAanvragenDidChange.bind(this)
	}

	bezwaarAanvragenDidChange = (value: boolean): void => {
		this.props.onBezwaarAangevraagd(this.props.afspraakId, value, this.props.isIngeschreven)
	}

	render(): JSX.Element {
		return <Paneel>
			<PaneelNaam titel={"Bezwaar"}/>
			<CheckboxValue label={"Bezwaar aanvragen"} checked={this.props.bezwaarAangevraagd}
						   handleChange={this.bezwaarAanvragenDidChange}
						   disabled={this.props.bezwaarDoorgevoerdOpCentraal}/>
		</Paneel>
	}

}