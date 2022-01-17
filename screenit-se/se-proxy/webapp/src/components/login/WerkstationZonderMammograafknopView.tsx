import React, {Component} from "react"
import {Button} from "reactstrap"
import {store} from "../../Store"
import {createActionSetHuidigeMammograaf} from "../../actions/MammograafActions"

export default class WerkstationZonderMammograafknopView extends Component<unknown> {

	constructor(props: unknown) {
		super(props)
		this.onClick = this.onClick.bind(this)
	}

	render(): JSX.Element {
		return <div>
			<Button className="btn-primary-se align-middle" onClick={this.onClick}>Werkstation zonder mammograaf</Button>
		</div>
	}

	onClick = (): void => {
		store.dispatch(createActionSetHuidigeMammograaf(0))
	}
}