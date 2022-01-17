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
import React from "react"
import * as Yup from "yup"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "../../../pages/bvo/cervix/ZasAanvragenPage.json"
import {Field, Formik} from "formik"
import SubmitForm from "../SubmitForm"
import {FormControl, FormControlLabel, Radio} from "@material-ui/core"
import styles from "./ZasNaUitstelForm.module.scss"
import {RadioGroup} from "formik-material-ui"
import {formatDateText} from "../../../utils/DateUtil"

export type ZasNaUitstelFormProps = {
	uitstellenTotDatum: Date | null,
	onSubmitSucces: (value: boolean) => void
}

const ZasNaUitstelForm = (props: ZasNaUitstelFormProps) => {

	type ZasNaUitstel = {
		zasNaUitstelOntvangen: string
	}

	const initialValues: ZasNaUitstel = {
		zasNaUitstelOntvangen: "",
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		zasNaUitstelOntvangen: Yup.string().required(getString(properties.form.error)),
	})

	return <>

		<Formik<ZasNaUitstel> initialValues={initialValues}
							  validationSchema={validatieSchema}
							  onSubmit={(values) => props.onSubmitSucces(values.zasNaUitstelOntvangen === "ja")
							  }>
			{formikProps => (
				<SubmitForm<ZasNaUitstel> title={getString(properties.form.title)}
										  formikProps={formikProps}
										  buttonLabel={getString(properties.form.button)}>
					<FormControl required
								 component="fieldset">

						<span>{getString(properties.form.description)}</span>

						<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{formikProps.errors.zasNaUitstelOntvangen}</p>

						<Field
							className={styles.radiobuttons}
							name="zasNaUitstelOntvangen"
							component={RadioGroup}
							value={formikProps.values.zasNaUitstelOntvangen || ""}>
							<ul>
								<li>
									<FormControlLabel
										className={styles.bevestigenRadioButton}
										value={"ja"}
										data-testid={"radio_ja"}
										control={<Radio/>}
										label={getString(properties.form.uitstellen.label, [formatDateText(props.uitstellenTotDatum)])}/>

									<p className={styles.checkboxTekst}><br/>{getString(properties.form.uitstellen.description)}<br/></p>
								</li>
								<li>
									<FormControlLabel
										value={"nee"}
										data-testid={"radio_nee"}
										control={<Radio/>}
										label={getString(properties.form.niet_uitstellen.label)}/>
									<p className={styles.checkboxTekst}><br/>{getString(properties.form.niet_uitstellen.description)}</p>
								</li>
							</ul>
						</Field>
					</FormControl>
				</SubmitForm>)}

		</Formik>
	</>
}

export default ZasNaUitstelForm
