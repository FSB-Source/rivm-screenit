import React, {Component} from "react"
import Select from "react-select"

export type MultiselectProps<T> = {
	id: string;
	options: MultiselectOption<T>[];
	disabled?: boolean;
	handleChange: (selectedOptions: T[]) => void;
	value?: MultiselectOption<T>[];
};

type MultiselectOption<T> = {
	value: T;
	label: string;
}

export default class MultiselectValue<T> extends Component<MultiselectProps<T>> {

	constructor(props: MultiselectProps<T>) {
		super(props)
		this.handleChange = this.handleChange.bind(this)
	}

	handleChange = (selectedOptions: readonly MultiselectOption<T>[]): void => {
		this.props.handleChange(selectedOptions ? selectedOptions.map(v => v.value) : [])
	}

	render(): JSX.Element {
		return <Select className={"select-container-lavendel multiselect-container"} id={this.props.id} style={{
			backgroundColor: "lavender",
			color: "#000!important",
		}} placeholder={"Kies..."} value={this.props.value} onChange={this.handleChange} options={this.props.options}
					   isDisabled={this.props.disabled} isMulti={true} isClearable={false}
					   noOptionsMessage={(): string => "Geen resultaten"}>
		</Select>
	}

}