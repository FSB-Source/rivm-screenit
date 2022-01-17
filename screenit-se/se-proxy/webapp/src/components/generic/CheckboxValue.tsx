import React, {ChangeEvent} from "react"
import {Form, FormGroup, Input, Label} from "reactstrap"

export type CheckboxValueProps = {
	label: string;
	checked: boolean;
	disabled: boolean;
	handleChange: (value: boolean) => void;
};

export default class CheckboxValue extends React.Component<CheckboxValueProps> {

	constructor(props: CheckboxValueProps) {
		super(props)
		this.handleChange.bind(this)
	}

	handleChange = (event: ChangeEvent<HTMLInputElement>): void => {
		if (event.target instanceof HTMLInputElement) {
			this.props.handleChange(event.target.checked)
		}
	}

	render(): JSX.Element {
		return <Form>
			<FormGroup check inline>
				<Label check>
					<Input type="checkbox" className="se-value" checked={this.props.checked}
						   onChange={this.handleChange} disabled={this.props.disabled}/>
					{this.props.label}
				</Label>
			</FormGroup>
		</Form>
	}

}