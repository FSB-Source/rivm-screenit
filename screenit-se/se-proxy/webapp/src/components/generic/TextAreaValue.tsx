import React, {ChangeEvent} from "react"
import {Input} from "reactstrap"

export type TextValueProps = {
	value: string;
	maxLength?: number;
	className?: string;
	placeholder?: string;
	disabled: boolean;
	color?: string;
	onChange: (value: string) => void;
};

type TextValueState = {
	value: string;
};

export default class TextAreaValue extends React.Component<TextValueProps, TextValueState> {

	constructor(props: TextValueProps) {
		super(props)
		this.state = {
			value: props.value,
		}
		this.updateValue = this.updateValue.bind(this)
	}

	componentDidUpdate(prevProps: TextValueProps): void {
		if (this.props.value !== prevProps.value && this.props.value !== this.state.value) {
			this.setState({
				value: this.props.value,
			})
		}
	}

	updateValue(event: ChangeEvent<HTMLInputElement>): void {
		const target = event.target

		if (target instanceof HTMLTextAreaElement) {
			this.setState({
				value: target.value,
			})
			this.props.onChange(target.value)
		}
	}

	render(): JSX.Element {
		return <div className={this.props.className}>
			<Input type={"textarea"} disabled={this.props.disabled} value={this.state.value}
				   placeholder={this.props.placeholder} onChange={this.updateValue}
				   maxLength={this.props.maxLength} style={{
				backgroundColor: this.props.color,
			}}/>
		</div>
	}

}