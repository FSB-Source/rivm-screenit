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
import properties from "../../../pages/bvo/colon/ColonAfspraakAfzeggenPage.json"
import {Field, Formik} from "formik"
import SubmitForm from "../SubmitForm"
import {FormControl, FormControlLabel, Radio} from "@material-ui/core"
import styles from "./ColonAfspraakAfzeggenForm.module.scss"
import {RadioGroup} from "formik-material-ui"
import RedenAfspraakAfzeggen from "../../../datatypes/colon/RedenAfspraakAfzeggen"

export type ColonAfspraakAfzeggenFormProps = {
	onSubmitSucces: (value: RedenAfspraakAfzeggen) => void
}

const ColonAfspraakAfzeggenForm = (props: ColonAfspraakAfzeggenFormProps) => {

	type RedenColonAfspraakAfzeggen = {
		afzegReden: string
	}

	const initialValues: RedenColonAfspraakAfzeggen = {
		afzegReden: "",
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		afzegReden: Yup.string().required(getString(properties.form.error)),
	})

	return <>

		<Formik<RedenColonAfspraakAfzeggen> initialValues={initialValues}
											validationSchema={validatieSchema}
											onSubmit={(values) => props.onSubmitSucces(values.afzegReden as RedenAfspraakAfzeggen)
											}>
			{formikProps => (
				<SubmitForm<RedenColonAfspraakAfzeggen> title={getString(properties.form.title)}
														formikProps={formikProps}
														buttonLabel={getString(properties.form.button)}>
					<FormControl
						required
						component="fieldset">

						<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{formikProps.errors.afzegReden}</p>

						<Field
							className={styles.radiobuttons}
							name="afzegReden"
							component={RadioGroup}
							value={formikProps.values.afzegReden || ""}>
							<ul>
								<li>
									<FormControlLabel
										className={styles.bevestigenRadioButton}
										value={RedenAfspraakAfzeggen.CLIENT_WIL_NIET_DEELNEMEN}
										data-testid={"radio_client_wil_niet_deelnemen"}
										control={<Radio/>}
										label={getString(properties.form.radiobutton.niet_deelnemen)}/>
								</li>
								<li>
									<FormControlLabel
										value={RedenAfspraakAfzeggen.HUISARTS_ADVISEERT_ANNULERING}
										data-testid={"radio_huisart_adviseert_annulering"}
										control={<Radio/>}
										label={getString(properties.form.radiobutton.huisarts_advies)}/>
								</li>
							</ul>
						</Field>
					</FormControl>
				</SubmitForm>)}

		</Formik>
	</>
}

export default ColonAfspraakAfzeggenForm
