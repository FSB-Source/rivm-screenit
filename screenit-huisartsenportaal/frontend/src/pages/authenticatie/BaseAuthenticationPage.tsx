/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import styles from "./BaseAuthenticationPage.module.scss"
import classNames from "classnames"
import {Button, Row} from "react-bootstrap"
import {Form, Formik, FormikProps, FormikValues} from "formik"
import React from "react"

export interface BaseAuthenticationPageProps<T> {
	title: string;
	description?: string;
	submitText: string;
	initialValues: T;
	onSubmit: (values: T) => void;
	validationSchema?: any;
	children: | ((props: FormikProps<T>) => React.ReactNode) | React.ReactNode;
}

function BaseAuthenticationPage<T extends FormikValues>(props: BaseAuthenticationPageProps<T>) {
	return <Row className={styles.style}>
		<div className={classNames(styles.panel, "mx-auto", "p-4", "rounded")}>
			<h3>{props.title}</h3>
			<hr/>
			{props.description && <p>{props.description}</p>}
			<Formik<T>
				initialValues={props.initialValues}
				validationSchema={props.validationSchema}
				onSubmit={props.onSubmit}>
				{(formikProps) => {
					return <Form className={styles.form}>
						{React.createElement(props.children as any, formikProps)}
						<hr/>
						<Button type="submit">{props.submitText}</Button>
					</Form>
				}}
			</Formik>
		</div>
	</Row>
}

export default BaseAuthenticationPage
