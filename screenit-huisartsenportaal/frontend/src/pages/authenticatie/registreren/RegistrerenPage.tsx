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
import BaseAuthenticationPage from "../BaseAuthenticationPage"
import {getString} from "../../../util/TekstPropertyUtil"
import properties from "./RegistrerenPage.json"
import validatieProperties from "../../../util/ValidatieUtil.json"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import {useAppThunkDispatch} from "../../../index"
import {authenticate} from "../../../api/AanmeldenThunkAction"
import {AuthenticationScope} from "../../../state/datatypes/enums/AuthenticationScope"
import {RegistrationDto} from "../../../state/datatypes/dto/RegistrationDto"
import {useNavigate} from "react-router"
import {createActionPushToast} from "../../../state/ToastsState"
import {ToastType} from "../../../state/datatypes/Toast"
import FormTextField from "../../../components/form/text/FormTextField"
import * as Yup from "yup"
import React from "react"

const RegistrerenPage = () => {
	const dispatch = useAppThunkDispatch()
	const navigate = useNavigate()

	return (
		<BaseAuthenticationPage<RegistrationDto>
			title={getString(properties.title)}
			submitText={getString(properties.form.buttons.submit)}
			initialValues={{
				agbCode: "",
				registratieCode: "",
			}}
			validationSchema={Yup.object({
				agbCode: Yup.string().required(getString(validatieProperties.required)),
				registratieCode: Yup.string().required(getString(validatieProperties.required)),
			})}
			onSubmit={(credentials => {
				dispatch(loadingThunkAction(authenticate(credentials.agbCode, credentials.registratieCode, AuthenticationScope.REGISTREREN))).then(() => {
					dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.toast.success)}))
					navigate("/registreren/voltooien")
				})
			})}>
			{(formikProps) => (<>
				<FormTextField className={"row my-3"} property={"agbCode"} error={formikProps.errors.agbCode} required
							   label={getString(properties.form.labels.agbCode)}/>
				<FormTextField className={"row my-3"} property={"registratieCode"} error={formikProps.errors.registratieCode} required
							   label={getString(properties.form.labels.registrationCode)}/>
			</>)}
		</BaseAuthenticationPage>
	)
}

export default RegistrerenPage
