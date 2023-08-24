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
import {useAppThunkDispatch} from "../../../../index"
import BaseAuthenticationPage from "../../BaseAuthenticationPage"
import {WachtwoordVergetenLoginDto} from "../../../../state/datatypes/dto/WachtwoordVergetenLoginDto"
import properties from "./WachtwoordResetCodePage.json"
import validatieProperties from "../../../../util/ValidatieUtil.json"
import {getString} from "../../../../util/TekstPropertyUtil"
import {loadingThunkAction} from "../../../../api/LoadingThunkAction"
import {authenticate} from "../../../../api/AanmeldenThunkAction"
import {AuthenticationScope} from "../../../../state/datatypes/enums/AuthenticationScope"
import {createActionPushToast} from "../../../../state/ToastsState"
import {ToastType} from "../../../../state/datatypes/Toast"
import FormTextField from "../../../../components/form/text/FormTextField"
import {useNavigate} from "react-router"
import * as Yup from "yup"
import React from "react"

const WachtwoordResetCodePage = () => {
	const dispatch = useAppThunkDispatch()
	const navigate = useNavigate()

	return (
		<BaseAuthenticationPage<WachtwoordVergetenLoginDto>
			title={getString(properties.title)}
			submitText={getString(properties.form.buttons.submit)}
			initialValues={{
				emailOfGebruikersnaam: "",
				inlogcode: "",
			}}
			validationSchema={Yup.object({
				emailOfGebruikersnaam: Yup.string().required(getString(validatieProperties.required)),
				inlogcode: Yup.string().required(getString(validatieProperties.required)),
			})}
			onSubmit={(loginDto => {
				dispatch(loadingThunkAction(authenticate(loginDto.emailOfGebruikersnaam, loginDto.inlogcode, AuthenticationScope.WACHTWOORDVERGETEN))).then(() => {
					dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.toast.success)}))
					navigate("/wachtwoordvergeten/voltooien")
				})
			})}>
			{(formikProps => <>
				<FormTextField className={"row my-3"} property={"emailOfGebruikersnaam"} error={formikProps.errors.emailOfGebruikersnaam} required
							   label={getString(properties.form.labels.emailOfGebruikersnaam)}/>
				<FormTextField className={"row my-3"} property={"inlogcode"} required error={formikProps.errors.inlogcode}
							   label={getString(properties.form.labels.inlogcode)}/>
			</>)}
		</BaseAuthenticationPage>
	)
}

export default WachtwoordResetCodePage
