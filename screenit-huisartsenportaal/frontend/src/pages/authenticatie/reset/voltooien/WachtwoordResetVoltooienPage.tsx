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
import {useAppSelector, useAppThunkDispatch} from "../../../../index"
import BaseAuthenticationPage from "../../BaseAuthenticationPage"
import properties from "./WachtwoordResetVoltooienPage.json"
import validatieProperties from "../../../../util/ValidatieUtil.json"
import {getString} from "../../../../util/TekstPropertyUtil"
import {loadingThunkAction} from "../../../../api/LoadingThunkAction"
import {createActionPushToast} from "../../../../state/ToastsState"
import {ToastType} from "../../../../state/datatypes/Toast"
import {useNavigate} from "react-router"
import FormPasswordTextField from "../../../../components/form/text/FormPasswordTextField"
import {WachtwoordWijzigenDto} from "../../../../state/datatypes/dto/WachtwoordWijzigenDto"
import {wachtwoordWijzigen} from "../../../../api/WachtwoordThunkAction"
import * as Yup from "yup"
import {afmelden} from "../../../../api/AfmeldenThunkAction"
import React from "react"
import {wachtwoordValidatie} from "../../../../util/ValidatieUtil"

const WachtwoordResetVoltooienPage = () => {
	const oauth = useAppSelector((state => state.oauth))
	const dispatch = useAppThunkDispatch()
	const navigate = useNavigate()

	return (
		<BaseAuthenticationPage<WachtwoordWijzigenDto>
			title={getString(properties.title)}
			submitText={getString(properties.form.buttons.submit)}
			initialValues={{
				nieuweWachtwoord: "",
				nieuweWachtwoordControle: "",
			}}
			validationSchema={Yup.object({
				nieuweWachtwoord: wachtwoordValidatie(getString(properties.form.labels.nieuweWachtwoord))
					.required(getString(validatieProperties.required)),
				nieuweWachtwoordControle: Yup.string()
					.oneOf([Yup.ref("nieuweWachtwoord"), undefined], getString(validatieProperties.wachtwoordControle))
					.required(getString(validatieProperties.required)),
			})}
			onSubmit={(wijzigenDto => {
				dispatch(loadingThunkAction(wachtwoordWijzigen(wijzigenDto))).then(() => {
					dispatch(afmelden(oauth!)).then(() => {
						dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.toast.success)}))
					})
					navigate("/")
				})
			})}>
			{(formikProps) => {
				return <>
					<FormPasswordTextField className={"row my-3"} property={"nieuweWachtwoord"} error={formikProps.errors.nieuweWachtwoord} required
										   label={getString(properties.form.labels.nieuweWachtwoord)}/>
					<FormPasswordTextField className={"row my-3"} property={"nieuweWachtwoordControle"} error={formikProps.errors.nieuweWachtwoordControle} required
										   label={getString(properties.form.labels.nieuweWachtwoordControle)}/>
				</>
			}}
		</BaseAuthenticationPage>
	)
}

export default WachtwoordResetVoltooienPage
