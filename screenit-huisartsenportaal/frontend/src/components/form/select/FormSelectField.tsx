/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import BaseFormField, {SharedFormFieldProps} from "../BaseFormField"
import Select from "react-select"
import classNames from "classnames"
import styles from "./FormSelectField.module.scss"
import {SelectOption} from "../../../state/datatypes/dto/SelectOption"

import React from "react"

export interface FormSelectFieldProps<T> extends SharedFormFieldProps {
	clearable?: boolean;
	options: SelectOption<T>[];
	value?: T | null;
	setValue: (value: T | null) => void;
	formatOptionLabel?: (data: SelectOption<T>) => JSX.Element;
}

function FormSelectField<T>(props: FormSelectFieldProps<T>) {
	return <BaseFormField {...props}>
		<Select<SelectOption<T>>
			isClearable={props.clearable}
			className={classNames(styles.style)}
			options={props.options}
			value={props.options.find(option => option.value === props.value)}
			getOptionLabel={(option) => option.label}
			onChange={(option) => props.setValue(option?.value || null)}
			placeholder={props.label}
			formatOptionLabel={props.formatOptionLabel}
			noOptionsMessage={() => "Geen opties beschikbaar"}
			isSearchable={false}
			isDisabled={props.disabled}
		/>
	</BaseFormField>
}

export default FormSelectField
