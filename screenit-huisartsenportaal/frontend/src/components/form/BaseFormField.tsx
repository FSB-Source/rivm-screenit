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
import classNames from "classnames"
import styles from "./BaseFormField.module.scss"
import {Col, FormGroup} from "react-bootstrap"
import React from "react"

export interface SharedFormFieldProps {
	alignRight?: boolean;
	property: string;
	className?: string;
	label: string;
	type?: string;
	required?: boolean;
	error?: string;
	disabled?: boolean;
	placeholder?: string;
}

export interface BaseFormFieldProps extends SharedFormFieldProps {
	children: JSX.Element;
}

const BaseFormField = (props: BaseFormFieldProps) => {
	return <FormGroup className={classNames(styles.style, props.alignRight && styles.alignRight, props.error && styles.error, props.className)}>
		<Col md={4}>
			<label className="col-form-label" htmlFor={props.property}>{props.label}{props.required && <sub>*</sub>}</label>
		</Col>
		<Col md={8}>
			{props.children}
			{props.error && <p className="form-error">{props.error}</p>}
		</Col>
	</FormGroup>
}

export default BaseFormField
