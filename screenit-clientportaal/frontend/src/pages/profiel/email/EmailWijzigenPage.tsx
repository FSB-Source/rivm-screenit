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
import {useThunkDispatch} from "../../.."
import {useNavigate} from "react-router-dom"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import * as Yup from "yup"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "./EmailWijzigenPage.json"
import {isEmailadresValidOfLeeg} from "../../../validators/EmailValidator"
import ActieBasePage from "../../ActieBasePage"
import {Formik} from "formik"
import {saveEmail} from "../../../api/EmailWijzigenThunkAction"
import {showToast} from "../../../utils/ToastUtil"
import SubmitForm from "../../../components/form/SubmitForm"
import ScreenitTextfield from "../../../components/input/ScreenitTextfield"
import styles from "./EmailWijzigenPage.module.scss"
import {EmailDto} from "../../../datatypes/mail/EmailDto"

const EmailWijzigenPage = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const persoon = useSelector((state: State) => state.client.persoon)

	const initialValues = {
		emailadres: persoon.emailadres || "",
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape(
		{
			emailadres: Yup.string().test("emailValidatie", getString(properties.form.invalid_mail_adres), function (value) {
				return isEmailadresValidOfLeeg(value)
			}),
		},
	)

	function updateEmailAdres(email: EmailDto) {
		dispatch(saveEmail(email)).then(() => {
			showToast(getString(properties.toast.title), "")
			navigate("/profiel")
		})
	}

	return (
		<ActieBasePage bvoName={""}
					   title={getString(properties.page.title)}
					   description={getString(properties.page.description)}
					   hintBegin={getString(properties.page.hint)}>

			<Formik initialValues={initialValues}
					validationSchema={validatieSchema}
					onSubmit={(email) => {
						updateEmailAdres(email)
					}}>

				{formikProps => (<SubmitForm title={getString(properties.form.title)}
											 formikProps={formikProps}
											 buttonLabel={getString(properties.form.button)}>
					<ScreenitTextfield name={"emailadres"}
									   value={formikProps.values.emailadres}
									   placeholder={getString(properties.form.mail)}
									   invalidMessage={formikProps.errors.emailadres}
									   onChange={value => formikProps.setFieldValue("emailadres", value)}
									   className={styles.emailInput}/>
				</SubmitForm>)}
			</Formik>
		</ActieBasePage>
	)
}

export default EmailWijzigenPage
