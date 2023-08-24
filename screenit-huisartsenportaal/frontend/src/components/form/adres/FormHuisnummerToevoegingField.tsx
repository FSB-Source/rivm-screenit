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
import classNames from "classnames"
import styles from "../BaseFormField.module.scss"
import {Col, FormGroup, Row} from "react-bootstrap"
import {SharedFormFieldProps} from "../BaseFormField"
import {Field} from "formik"
import React from "react"

export interface FormHuisnummerToevoegingFieldProps extends SharedFormFieldProps {
	toevoegingProperty: string;
	huisnummerPlaceholder: string;
	toevoegingPlaceholder: string;
}

const FormHuisnummerToevoegingField = (props: FormHuisnummerToevoegingFieldProps) => {
	return <FormGroup className={classNames(styles.style, props.alignRight && styles.alignRight, props.error && styles.error, props.className)}>
		<Col md={4}>
			<label className="col-form-label" htmlFor={props.property}>{props.label}{props.required && <sub>*</sub>}</label>
		</Col>
		<Col md={8}>
			<Row>
				<Col md={6}>
					<Field className="form-control" id={props.property} disabled={props.disabled} required={props.required} type={"number"} min={0} name={props.property}
						   placeholder={props.huisnummerPlaceholder}/>
				</Col>
				<Col md={6}>
					<Field className="form-control" id={props.toevoegingProperty} disabled={props.disabled} name={props.toevoegingProperty}
						   placeholder={props.toevoegingPlaceholder}/>
				</Col>
			</Row>
			{props.error && <p className="form-error">{props.error}</p>}
		</Col>
	</FormGroup>
}

export default FormHuisnummerToevoegingField
