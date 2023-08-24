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
import styles from "./CervixUitstelAanvragenPage.module.scss"
import {getString} from "../../../utils/TekstPropertyUtil"
import {CervixUitstelType} from "../../../datatypes/cervix/CervixUitstelType"
import {Formik} from "formik"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import SubmitForm from "../../../components/form/SubmitForm"
import {CervixUitstel, CervixUitstelFormulier} from "../../../datatypes/cervix/CervixUitstelDto"
import properties from "./CervixUitstelAanvragenPage.json"
import {formatDate, getAantalDagenTussenDatums, getBovengrensUitLijst, minDagen, parseIsoDatumNederlandseIso, plusDagen, plusMaanden, vandaag} from "../../../utils/DateUtil"
import ScreenitDatePicker from "../../../components/input/ScreenitDatePicker"
import * as Yup from "yup"
import {CervixUitstelStatus} from "../../../datatypes/cervix/CervixUitstelStatus"

export type CervixUitstelAanvragenFormProps = {
	cervixUitstel: CervixUitstel;
	uitstelStatus: CervixUitstelStatus,
	geboortedatumDisplay: string,
	onSubmitSucces: (value: CervixUitstelFormulier) => void

}

const CervixUitstelAanvragenForm = (props: CervixUitstelAanvragenFormProps) => {
	const geboortedatum = parseIsoDatumNederlandseIso(props.geboortedatumDisplay)
	const datumVanDertigsteVerjaardag = new Date(geboortedatum.setFullYear(geboortedatum.getFullYear() + 30))
	const duurZwangerschap = 9
	const periodeTussenVandaagEnDertigsteVerjaardag = getAantalDagenTussenDatums(datumVanDertigsteVerjaardag, vandaag())

	const minPeriodeTussenZwangerEnOnderzoek = props.uitstelStatus ? props.uitstelStatus.uitstelBijZwangerschap : 42
	const periodeTussenZwangerschapEnDertigsteVerjaardag = periodeTussenVandaagEnDertigsteVerjaardag > 0 ? minPeriodeTussenZwangerEnOnderzoek - periodeTussenVandaagEnDertigsteVerjaardag : minPeriodeTussenZwangerEnOnderzoek

	const minDatumZwangerschap = minDagen(vandaag(), periodeTussenZwangerschapEnDertigsteVerjaardag)
	const maxDatumZwangerschap = plusMaanden(vandaag(), duurZwangerschap)
	const minDatumAnders = getBovengrensUitLijst([plusDagen(datumVanDertigsteVerjaardag, 1), plusDagen(vandaag(), 1)])
	const maxDatumAnders = props.uitstelStatus.datumVolgendeRonde != null ? props.uitstelStatus.datumVolgendeRonde : plusMaanden(vandaag(), 60)

	function getInitialValueUitstellenTotDatum() {
		if (props.cervixUitstel.uitstelType === CervixUitstelType.ZWANGERSCHAP && props.cervixUitstel.uitstellenTotDatum) {
			return minDagen(props.cervixUitstel.uitstellenTotDatum, minPeriodeTussenZwangerEnOnderzoek)
		} else {
			return props.cervixUitstel.uitstellenTotDatum || null
		}
	}

	const initialValues: CervixUitstelFormulier = {
		uitstelType: props.cervixUitstel.uitstelType,
		uitgerekendeDatum: getInitialValueUitstellenTotDatum(),
		uitstellenTotDatum: getInitialValueUitstellenTotDatum(),
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		uitstelType: Yup.string().required(getString(properties.form.error.verplicht.radiobutton)),
		uitgerekendeDatum: Yup.date().when("uitstelType", {
			is: CervixUitstelType.ZWANGERSCHAP,
			then: (schema) => schema.required(getString(properties.form.error.verplicht.datepicker))
				.typeError(getString(properties.form.error.datum.ongeldig))
				.min(minDatumZwangerschap, getString(properties.form.error.datum.minimum, [formatDate(minDatumZwangerschap)]))
				.max(maxDatumZwangerschap, getString(properties.form.error.datum.maximum, [formatDate(maxDatumZwangerschap)])),
			otherwise: (schema) => schema.nullable(),
		}),
		uitstellenTotDatum: Yup.date().when("uitstelType", {
			is: CervixUitstelType.ANDERS,
			then: (schema) => schema.required(getString(properties.form.error.verplicht.datepicker))
				.typeError(getString(properties.form.error.datum.ongeldig))
				.min(minDatumAnders, getString(properties.form.error.datum.minimum, [formatDate(minDatumAnders)]))
				.max(maxDatumAnders, getString(properties.form.error.datum.maximum, [formatDate(maxDatumAnders)])),
			otherwise: (schema) => schema.nullable(),
		}),
	})

	return <>
		<Formik<CervixUitstelFormulier> initialValues={initialValues}
										validationSchema={validatieSchema}
										onSubmit={(values) => {
											props.onSubmitSucces({
												...values,
												uitgerekendeDatum: values.uitgerekendeDatum == null ? null : new Date(values.uitgerekendeDatum),
												uitstellenTotDatum: values.uitstellenTotDatum == null ? null : new Date(values.uitstellenTotDatum),
											})
										}}>
			{formikProps => (
				<SubmitForm<CervixUitstelFormulier> title={getString(properties.form.title)}
													formikProps={formikProps}
													buttonLabel={getString(properties.form.button)}>
					<FormControl variant="standard" required component="fieldset">
						<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{formikProps.errors.uitstelType}</p>
						<RadioGroup
							name="uitstelType"
							onChange={formikProps.handleChange}
							value={formikProps.values.uitstelType || ""}>
							<ul>
								<li className={styles.radioGroup}>
									<FormControlLabel
										value={CervixUitstelType.ZWANGERSCHAP}
										data-testid={"radio_zwangerschap"}
										control={<Radio/>}
										label={getString(properties.form.radiobutton.zwanger)}/>
									<FormControlLabel
										value={CervixUitstelType.ANDERS}
										data-testid={"radio_anders"}
										control={<Radio/>}
										label={getString(properties.form.radiobutton.anders)}/>
								</li>
							</ul>
						</RadioGroup>
					</FormControl>

					{formikProps.values.uitstelType === CervixUitstelType.ZWANGERSCHAP &&
						<ScreenitDatePicker className={styles.datepicker}
											propertyName={"uitgerekendeDatum"}
											label={getString(properties.form.placeholder.zwanger)}
											title={getString(properties.form.label.zwanger)}
											value={formikProps.values.uitgerekendeDatum}
											errorLabel={formikProps.errors.uitgerekendeDatum}
											onChange={value => {
												formikProps.setFieldValue("uitgerekendeDatum", value)
											}}/>}

					{formikProps.values.uitstelType === CervixUitstelType.ANDERS &&
						<ScreenitDatePicker className={styles.datepicker}
											propertyName={"uitstellenTotDatum"}
											label={getString(properties.form.placeholder.anders)}
											title={getString(properties.form.label.anders)}
											value={formikProps.values.uitstellenTotDatum}
											errorLabel={formikProps.errors.uitstellenTotDatum}
											onChange={value => {
												formikProps.setFieldValue("uitstellenTotDatum", value)
											}}/>}
				</SubmitForm>)}
		</Formik>

	</>
}
export default CervixUitstelAanvragenForm
