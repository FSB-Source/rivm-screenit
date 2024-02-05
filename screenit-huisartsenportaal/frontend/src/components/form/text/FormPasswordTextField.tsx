/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {Field} from "formik"
import BaseFormField from "../BaseFormField"
import React, {useState} from "react"
import {FormTextFieldProps} from "./FormTextField"
import styles from "./FormPasswordTextField.module.scss"

const FormPasswordTextField = (props: FormTextFieldProps) => {
	const [visible, setVisible] = useState<boolean>(false)

	return <BaseFormField {...props}>
		<div className={styles.style}>
			<i className={"bi bi-eye-" + (visible ? "slash-" : "") + "fill"} onMouseDown={() => setVisible(true)} onMouseUp={() => setVisible(false)}
			   onMouseLeave={() => setVisible(false)}/>
			<Field className="form-control" id={props.property} min={props.min} required={props.required} max={props.max} maxLength={props.maxLength} name={props.property}
				   type={visible ? "text" : "password"} placeholder={props.placeholder ? props.placeholder : props.label} disabled={props.disabled}/>
		</div>
	</BaseFormField>
}

export default FormPasswordTextField
