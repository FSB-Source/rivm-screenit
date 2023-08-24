/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import React from "react"
import classNames from "classnames"
import styles from "./ScreenitDropdown.module.scss"
import {FormControl, InputLabel, MenuItem, Select, SelectChangeEvent} from "@mui/material"

export type DropdownOption = {
	value: string,
	label: string
}

export type ScreenitDropdownProps = {
	className?: string,
	options: DropdownOption[],
	value?: string,
	initialValue?: string,
	placeholder: string,
	propertyName: string,
	invalidMessage?: string,
	onChange: (
		event: SelectChangeEvent<string>,
		child: React.ReactNode,
	) => void;
}

function ScreenitDropdown(props: ScreenitDropdownProps) {
	return (
		<div className={styles.container}>
			<FormControl className={classNames(styles.inputContainer)}>
				<InputLabel id={"lbl_" + props.propertyName} htmlFor={props.propertyName}>{props.placeholder}</InputLabel>
				<Select
					className={styles.dropdown}
					labelId={"lbl_" + props.propertyName}
					inputProps={{
						name: props.placeholder,
						id: props.propertyName,
					}}
					variant={"standard"}
					placeholder={props.placeholder}
					defaultValue={props.initialValue ? props.initialValue : ""}
					value={props.value ? props.value : ""}
					onChange={props.onChange}>

					{props.options.map((option, key) =>
						<MenuItem key={key} value={option.value}>{option.label}</MenuItem>,
					)}
				</Select>
			</FormControl>
			{props.invalidMessage && <label className={styles.errorLabel}>{props.invalidMessage}</label>}
		</div>
	)
}

export default ScreenitDropdown
