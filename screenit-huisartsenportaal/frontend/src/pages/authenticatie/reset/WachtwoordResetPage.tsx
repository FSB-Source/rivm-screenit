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
import BaseAuthenticationPage from "../BaseAuthenticationPage"
import {getString} from "../../../util/TekstPropertyUtil"
import properties from "./WachtwoordResetPage.json"
import {useAppThunkDispatch} from "../../../index"
import {createActionPushToast} from "../../../state/ToastsState"
import {ToastType} from "../../../state/datatypes/Toast"
import FormTextField from "../../../components/form/text/FormTextField"
import {WachtwoordVergetenDto} from "../../../state/datatypes/dto/WachtwoordVergetenDto"
import {wachtwoordVergeten} from "../../../api/WachtwoordThunkAction"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import React from "react"

const WachtwoordResetPage = () => {
	const dispatch = useAppThunkDispatch()

	return (
		<BaseAuthenticationPage<WachtwoordVergetenDto>
			title={getString(properties.title)}
			submitText={getString(properties.form.buttons.submit)}
			initialValues={{
				gebruikersnaam: "",
				email: "",
			}}

			onSubmit={(vergetenDto => {
				dispatch(loadingThunkAction(wachtwoordVergeten(vergetenDto))).then(() => {
					dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.toast.success)}))
				})
			})}>
			{() => (
				<>
					<FormTextField className={"row my-3"} property={"gebruikersnaam"} required label={getString(properties.form.labels.gebruikersnaam)}/>
					<FormTextField className={"row my-3"} property={"email"} required label={getString(properties.form.labels.email)}/>
				</>
			)}
		</BaseAuthenticationPage>
	)
}

export default WachtwoordResetPage
