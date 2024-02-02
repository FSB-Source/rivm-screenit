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
import BaseFormField, {SharedFormFieldProps} from "../BaseFormField"
import React from "react"

export interface FormTextFieldProps extends SharedFormFieldProps {
	min?: number;
	max?: number;
	maxLength?: number;
}

const FormTextField = (props: FormTextFieldProps) => {
	return <BaseFormField {...props}>
		<Field className="form-control" id={props.property} min={props.min} max={props.max} maxLength={props.maxLength} name={props.property} type={props.type && props.type}
			   placeholder={props.placeholder ? props.placeholder : props.label} disabled={props.disabled} required={props.required}/>
	</BaseFormField>
}

export default FormTextField
