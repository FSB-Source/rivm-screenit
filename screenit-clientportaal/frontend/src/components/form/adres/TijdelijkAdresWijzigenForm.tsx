/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import * as Yup from "yup"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "../../../pages/profiel/TijdelijkAdresWijzigenPage.json"
import {Formik} from "formik"
import SubmitForm from "../SubmitForm"
import React from "react"
import {TijdelijkAdres} from "../../../datatypes/adres/TijdelijkAdres"
import ScreenitTextfield from "../../input/ScreenitTextfield"
import ScreenitDatePicker from "../../input/ScreenitDatePicker"
import {formatDate, isDatumInToekomst, isDatumVandaagOfLater, plusDagen, vandaag} from "../../../utils/DateUtil"
import styles from "./TijdelijkAdresWijzigenForm.module.scss"
import {Col, Row} from "react-bootstrap"
import {REGEX_HUISLETTER, REGEX_HUISNUMMER, REGEX_POSTCODE_EXACT} from "../../../validators/AdresValidator"
import classNames from "classnames"

export type TijdelijkAdresWijzigenFormProps = {
	huidigTijdelijkAdres?: TijdelijkAdres
	onSubmitSucces: (value: TijdelijkAdres) => void
}

const TijdelijkAdresWijzigenForm = (props: TijdelijkAdresWijzigenFormProps) => {
	const initialValues = props.huidigTijdelijkAdres && (!props.huidigTijdelijkAdres.eindDatum || isDatumVandaagOfLater(props.huidigTijdelijkAdres.eindDatum)) ? props.huidigTijdelijkAdres : {
		straat: "",
		huisnummer: "" as any,
		huisletter: "",
		huisnummerToevoeging: "",
		huisnummerAanduiding: "",
		postcode: "",
		plaats: "",
		startDatum: null as any,
		eindDatum: null,
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		straat: Yup.string().max(43, getString(properties.form.error.lengte))
			.required(getString(properties.form.error.verplicht)),
		huisnummer: Yup.string().required(getString(properties.form.error.verplicht))
			.max(10, getString(properties.form.error.lengte))
			.matches(REGEX_HUISNUMMER, getString(properties.form.error.huisnummer)),
		huisletter: Yup.string().max(1, getString(properties.form.error.lengte))
			.matches(REGEX_HUISLETTER, getString(properties.form.error.huisletter)),
		huisnummerToevoeging: Yup.string().max(26, getString(properties.form.error.lengte)),
		huisnummerAanduiding: Yup.string().max(2, getString(properties.form.error.lengte)),
		postcode: Yup.string().required(getString(properties.form.error.verplicht))
			.max(6, getString(properties.form.error.lengte))
			.matches(REGEX_POSTCODE_EXACT, getString(properties.form.error.postcode)),
		plaats: Yup.string().max(30, getString(properties.form.error.lengte))
			.required(getString(properties.form.error.verplicht)),
		startDatum: Yup.date()
			.test("datumTest", getString(properties.form.error.datum.eind_voor_start), function (value) {
				return this.parent.eindDatum ? this.parent.eindDatum > value! : true
			})
			.test("verledenTest", getString(properties.form.error.datum.minimum, [formatDate(plusDagen(vandaag(), 1))]), (value) => {
				return (value !== undefined && ((props.huidigTijdelijkAdres != null && (+value === +props.huidigTijdelijkAdres.startDatum)) || isDatumInToekomst(value)))
			})
			.nullable()
			.required(getString(properties.form.error.verplicht))
			.typeError(getString(properties.form.error.datum.ongeldig)),
		eindDatum: Yup.date()
			.test("datumTest", getString(properties.form.error.datum.eind_voor_start), function (value) {
				return value ? this.parent.startDatum < value : true
			})
			.test("verplichtTest", getString(properties.form.error.verplicht), function (value) {
				return (props.huidigTijdelijkAdres != null && props.huidigTijdelijkAdres.eindDatum === null) || value != null
			})
			.nullable()
			.min(plusDagen(vandaag(), 1), getString(properties.form.error.datum.minimum, [formatDate(plusDagen(vandaag(), 1))]))
			.typeError(getString(properties.form.error.datum.ongeldig)),
	})

	return <>
		<Formik<TijdelijkAdres> initialValues={initialValues}
								validationSchema={validatieSchema}
								onSubmit={(values) =>
									props.onSubmitSucces({
										...values,
										id: props.huidigTijdelijkAdres ? props.huidigTijdelijkAdres.id : undefined,
										huisnummer: Number(values.huisnummer),
									})
								}>
			{formikProps => (
				<SubmitForm title={getString(properties.form.title.algemeen)}
							formikProps={formikProps}
							buttonLabel={getString(properties.form.button)}>
					<div className={styles.formContent}>
						<Row>
							<Col xs={12} className={styles.inputColumn}>
								<ScreenitTextfield name={"straat"}
												   placeholder={getString(properties.form.placeholder.straat)}
												   invalidMessage={formikProps.errors.straat}
												   value={formikProps.values.straat}
												   onChange={value => formikProps.setFieldValue("straat", value)}/>
							</Col>
						</Row>
						<Row>
							<Col xs={4} className={classNames(styles.inputColumn, styles.huisnummer)}>
								<ScreenitTextfield name={"huisnummer"}
												   placeholder={getString(properties.form.placeholder.huisnummer)}
												   invalidMessage={formikProps.errors.huisnummer}
												   value={String(formikProps.values.huisnummer)}
												   onChange={value => formikProps.setFieldValue("huisnummer", value)}/>
							</Col>
							<Col xs={2} className={classNames(styles.inputColumn, styles.huisletter)}>
								<ScreenitTextfield name={"huisletter"}
												   placeholder={getString(properties.form.placeholder.letter)}
												   invalidMessage={formikProps.errors.huisletter}
												   value={formikProps.values.huisletter}
												   onChange={value => formikProps.setFieldValue("huisletter", value)}/>
							</Col>
							<Col xs={3} className={classNames(styles.inputColumn, styles.huisnummerToevoeging)}>

								<ScreenitTextfield name={"huisnummerToevoeging"}
												   placeholder={getString(properties.form.placeholder.toevoeging)}
												   invalidMessage={formikProps.errors.huisnummerToevoeging}
												   value={formikProps.values.huisnummerToevoeging}
												   onChange={value => formikProps.setFieldValue("huisnummerToevoeging", value)}/>
							</Col>
							<Col xs={3} className={classNames(styles.inputColumn, styles.huisnummerAanduiding)}>
								<ScreenitTextfield name={"huisnummerAanduiding"}
												   placeholder={getString(properties.form.placeholder.aanduiding)}
												   invalidMessage={formikProps.errors.huisnummerAanduiding}
												   value={formikProps.values.huisnummerAanduiding}
												   onChange={value => formikProps.setFieldValue("huisnummerAanduiding", value)}/>
							</Col>
						</Row>
						<Row>
							<Col xs={3} className={classNames(styles.inputColumn, styles.postcode)}>
								<ScreenitTextfield name={"postcode"}
												   placeholder={getString(properties.form.placeholder.postcode)}
												   invalidMessage={formikProps.errors.postcode}
												   value={formikProps.values.postcode}
												   onChange={value => formikProps.setFieldValue("postcode", value)}/>
							</Col>
							<Col xs={9} className={classNames(styles.inputColumn, styles.plaats)}>
								<ScreenitTextfield name={"plaats"}
												   placeholder={getString(properties.form.placeholder.plaats)}
												   invalidMessage={formikProps.errors.plaats}
												   value={formikProps.values.plaats}
												   onChange={value => formikProps.setFieldValue("plaats", value)}/>
							</Col>
						</Row>

						<div>
							<span>{getString(properties.form.title.periode)}</span>
						</div>
						<Row>
							<Col xs={6} className={classNames(styles.inputColumn, styles.datum)}>
								<ScreenitDatePicker propertyName={"startDatum"}
													label={getString(properties.form.placeholder.startdatum)}
													value={formikProps.values.startDatum}
													errorLabel={formikProps.errors.startDatum}
													onChange={value => {
														formikProps.setFieldValue("startDatum", value)
													}}/>
							</Col>
							<Col xs={6} className={classNames(styles.inputColumn, styles.datum)}>
								<ScreenitDatePicker propertyName={"eindDatum"}
													label={getString(properties.form.placeholder.einddatum)}
													value={formikProps.values.eindDatum}
													errorLabel={formikProps.errors.eindDatum}
													onChange={value => {
														formikProps.setFieldValue("eindDatum", value)
													}}/>
							</Col>
						</Row>
					</div>
				</SubmitForm>)}

		</Formik>
	</>

}

export default TijdelijkAdresWijzigenForm
