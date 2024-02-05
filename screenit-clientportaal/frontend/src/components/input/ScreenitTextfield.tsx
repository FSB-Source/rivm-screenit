/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import styles from "./ScreenitTextfield.module.scss"
import React, {ChangeEvent, useEffect, useState} from "react"
import classNames from "classnames"

export type TextfieldProps = {
	className?: string
	name: string,
	placeholder: string,
	value?: string,
	invalidMessage?: string,
	onChange: (value: string) => void;
}

const ScreenitTextfield = (props: TextfieldProps) => {
	const [active, setActive] = useState<boolean>(false)

	useEffect(() => {
		if (props.value !== undefined && props.value !== "") {
			setActive(true)
		}
	}, [props.value])

	const activateField = () => {
		setActive(true)
	}

	const disableField = () => {
		if (props.value === "") {
			setActive(false)
		}
	}

	const updateInputValue = (e: ChangeEvent<HTMLInputElement>) => {
		props.onChange(e.target.value)
		activateField()
	}

	return (
		<div className={classNames(props.className, styles.inputDiv, props.invalidMessage && styles.inputInvalid)}>
			<label className={active ? styles.labelActive : styles.labelInactive} htmlFor={props.name}>{props.placeholder}</label>

			<input data-testid={"input_" + props.name}
				   className={styles.inputField} type="text" id={props.name} name={props.name}
				   autoComplete={"off"}
				   value={props.value}
				   onFocus={activateField}
				   onBlur={disableField}
				   onChange={updateInputValue}/>

			{props.invalidMessage && <label data-testid={"error_" + props.name} className={styles.errorLabel}>{props.invalidMessage}</label>}
		</div>
	)

}

export default ScreenitTextfield
