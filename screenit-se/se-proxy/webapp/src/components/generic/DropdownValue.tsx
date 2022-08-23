import React from "react"
import Select from "react-select"

export type DropdownValueProps<T> = {
	id: string;
	value?: T;
	disabled: boolean;
	options: Array<T>;
	required?: boolean;
	isWhite?: boolean;
	placeholder?: string;
	lavendel?: boolean;
	valueToLabel?: (value: T) => string;
	handleChange: (value?: T) => void;
};

type DropdownOption<T> = {
	value: T;
	label: string;
}

export default class DropdownValue<T> extends React.Component<DropdownValueProps<T>> {

	constructor(props: DropdownValueProps<T>) {
		super(props)
		this.handleChange = this.handleChange.bind(this)
	}

	handleChange = (selectedOption: DropdownOption<T> | null): void => {
		this.props.handleChange(selectedOption && selectedOption.value ? selectedOption.value : undefined)
	}

	render(): JSX.Element {
		return <div
			className={this.props.lavendel ? "select-container-lavendel" : ""}
			title={this.props.value ? (this.props.valueToLabel ? this.props.valueToLabel(this.props.value) : String(this.props.value)) : ""}>
			<Select id={this.props.id}
					styles={{
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
							backgroundColor: this.props.isWhite ? "#fff" : "lavender",
						}),
						option: (provided): any => ({
							...provided,
							":hover": !this.props.isWhite && {
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
							backgroundColor: this.props.isWhite ? "#fff" : "lavender",
							color: "#000!important",
						}),
					}} placeholder={this.props.placeholder ? this.props.placeholder : "Kies..."} value={this.props.value ? {
				value: this.props.value,
				label: this.props.valueToLabel ? this.props.valueToLabel(this.props.value) : String(this.props.value),
			} : null} onChange={this.handleChange} options={this.props.options.map(v => {
				return {
					value: v,
					label: this.props.valueToLabel ? this.props.valueToLabel(v) : String(v),
				}
			})} isDisabled={this.props.disabled} isClearable={!this.props.required} noOptionsMessage={(): string => "Geen resultaten"}/>
		</div>
	}

}