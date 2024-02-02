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
import React from "react"
import * as Yup from "yup"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "../../../pages/bvo/gedeeld/HeraanmeldenPage.json"
import {Formik} from "formik"
import SubmitForm from "../SubmitForm"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import styles from "./HeraanmeldenUitnodigingAanvragenForm.module.scss"

export type HeraanmeldenUitnodigingAanvragenFormProps = {
	onSubmitSucces: (value: boolean) => void
}

const HeraanmeldenUitnodigingAanvragenForm = (props: HeraanmeldenUitnodigingAanvragenFormProps) => {

	type UitnodigingColonAanvragen = {
		uitnodigingAanvragen: string
	}

	const initialValues: UitnodigingColonAanvragen = {
		uitnodigingAanvragen: "",
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		uitnodigingAanvragen: Yup.string().required(getString(properties.form.error)),
	})

	return <>

		<Formik<UitnodigingColonAanvragen> initialValues={initialValues}
										   validationSchema={validatieSchema}
										   onSubmit={(values) => props.onSubmitSucces(values.uitnodigingAanvragen === "ja")
										   }>
			{formikProps => (<SubmitForm<UitnodigingColonAanvragen> title={getString(properties.form.title.COLON.uitnodiging)}
																	formikProps={formikProps}
																	buttonLabel={getString(properties.form.button)}>
				<FormControl variant="standard" required component="fieldset">

					<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{formikProps.errors.uitnodigingAanvragen}</p>

					<RadioGroup
						name="uitnodigingAanvragen"
						onChange={formikProps.handleChange}
						value={formikProps.values.uitnodigingAanvragen || ""}>
						<ul className={styles.radioGroup}>
							<FormControlLabel
								className={styles.bevestigenRadioButton}
								value={"ja"}
								data-testid={"radio_ja"}
								control={<Radio/>}
								label={getString(properties.form.radio_button.ja)}/>
							<FormControlLabel
								value={"nee"}
								data-testid={"radio_nee"}
								control={<Radio/>}
								label={getString(properties.form.radio_button.nee)}/>
						</ul>
					</RadioGroup>
				</FormControl>
			</SubmitForm>)}

		</Formik>
	</>;
}

export default HeraanmeldenUitnodigingAanvragenForm
