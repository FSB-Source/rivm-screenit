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
		return <Select className={"select-container-lavendel multiselect-container"} id={this.props.id} styles={{
			placeholder: (provided): any => ({
				...provided,
				color: "black",
			}),
			dropdownIndicator: (provided): any => ({
				...provided,
				color: "black",
			}),
			clearIndicator: (provided): any => ({
				...provided,
				color: "black",
			}),
			menu: (provided): any => ({
				...provided,
				backgroundColor: "lavender",
			}),
			option: (provided): any => ({
				...provided,
				":hover": {
					backgroundColor: "#D8C1FB",
					color: "#54026e",
					cursor: "pointer",
				},
			}),
			container: (provided): any => ({
				...provided,
				width: "auto",
				color: "black",
			}),
			control: (provided): any => ({
				...provided,
				backgroundColor: "lavender",
				color: "#000!important",
			}),
			multiValue: (provided): any => ({
				...provided,
				backgroundColor: "#D8C1FB",
			}),
		}} placeholder={"Kies..."} value={this.props.value} onChange={this.handleChange} options={this.props.options}
					   isDisabled={this.props.disabled} isMulti={true} isClearable={false}
					   noOptionsMessage={(): string => "Geen resultaten"}/>
	}

}