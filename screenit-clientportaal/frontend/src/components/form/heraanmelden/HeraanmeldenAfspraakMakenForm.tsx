/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {Formik} from "formik"
import SubmitForm from "../SubmitForm"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import styles from "./HeraanmeldenAfspraakMakenForm.module.scss"
import properties from "../../../pages/bvo/gedeeld/HeraanmeldenPage.json"

export type HeraanmeldenAfspraakMakenFormProps = {
	onSubmitSucces: (value: boolean) => void
}

const HeraanmeldenAfspraakMakenForm = (props: HeraanmeldenAfspraakMakenFormProps) => {

	type NieuweAfspraakMaken = {
		afspraakMaken: string
	}

	const initialValues = {
		afspraakMaken: "",
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		afspraakMaken: Yup.string().required(getString(properties.form.error)),
	})

	return <>
		<Formik<NieuweAfspraakMaken> initialValues={initialValues}
									validationSchema={validatieSchema}
									onSubmit={(values) => props.onSubmitSucces(values.afspraakMaken === "ja")}>
			{formikProps => (
				<SubmitForm<NieuweAfspraakMaken>
					title={getString(properties.form.title.COLON.afspraak)}
												formikProps={formikProps}
												buttonLabel={getString(properties.form.button)}>
					<FormControl variant="standard" required component="fieldset">

						<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{formikProps.errors.afspraakMaken}</p>

						<RadioGroup
							name="afspraakMaken"
							onChange={formikProps.handleChange}>
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

export default HeraanmeldenAfspraakMakenForm
