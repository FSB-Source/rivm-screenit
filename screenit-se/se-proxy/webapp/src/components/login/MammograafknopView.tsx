import React, {Component} from "react"
import {Mammograaf} from "../../datatypes/Mammograaf"
import {Button} from "reactstrap"
import {store} from "../../Store"
import {createActionSetHuidigeMammograaf} from "../../actions/MammograafActions"

export type MammograafknopProps = {
	mammograaf: Mammograaf;
};

export default class MammograafknopView extends Component<MammograafknopProps> {
	constructor(props: MammograafknopProps) {
		super(props)
		this.onClick = this.onClick.bind(this)
	}

	render(): JSX.Element {
		const mammograaf: Mammograaf = this.props.mammograaf
		return <div>
			<Button className="btn-primary-se align-middle" onClick={this.onClick}>Werkstation bij mammograaf {mammograaf.aeTitle}</Button>
		</div>
	}

	onClick = (): void => {
		store.dispatch(createActionSetHuidigeMammograaf(this.props.mammograaf.id))
	}
}