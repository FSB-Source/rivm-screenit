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
import {Form, Formik} from "formik"
import {Button, Modal} from "react-bootstrap"
import properties from "./WachtwoordWijzigenModal.json"
import validatieProperties from "./../../../util/ValidatieUtil.json"
import * as Yup from "yup"
import {getString} from "../../../util/TekstPropertyUtil"
import {useAppThunkDispatch} from "../../../index"
import {WachtwoordWijzigenDto} from "../../../state/datatypes/dto/WachtwoordWijzigenDto"
import {wachtwoordWijzigen} from "../../../api/WachtwoordThunkAction"
import FormPasswordTextField from "../../form/text/FormPasswordTextField"
import {createActionPushToast} from "../../../state/ToastsState"
import {ToastType} from "../../../state/datatypes/Toast"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import {wachtwoordValidatie} from "../../../util/ValidatieUtil"
import React from "react"

export interface WachtwoordWijzigenModalProps {
	show: boolean;
	onHide: () => void;
}

const WachtwoordWijzigenModal = (props: WachtwoordWijzigenModalProps) => {
	const dispatch = useAppThunkDispatch()

	return (<Modal size={"lg"} show={props.show} onHide={props.onHide}>
		<Modal.Header closeButton>
			<Modal.Title>{getString(properties.title)}</Modal.Title>
		</Modal.Header>
		<Formik<WachtwoordWijzigenDto>
			initialValues={{
				oudeWachtwoord: "",
				nieuweWachtwoord: "",
				nieuweWachtwoordControle: "",
			}}
			validationSchema={Yup.object().shape({
				oudeWachtwoord: Yup.string()
					.required(getString(validatieProperties.required)),
				nieuweWachtwoord: wachtwoordValidatie(getString(properties.form.fields.nieuweWachtwoord))
					.required(getString(validatieProperties.required)),
				nieuweWachtwoordControle: Yup.string()
					.oneOf([Yup.ref("nieuweWachtwoord"), undefined], getString(validatieProperties.wachtwoordControle))
					.required(getString(validatieProperties.required)),
			})}
			onSubmit={wachtwoordDto => {
				dispatch(loadingThunkAction(wachtwoordWijzigen(wachtwoordDto))).then(() => {
					dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.toast.success)}))
					props.onHide()
				})
			}}
		>
			{formikProps => <Form>
				<Modal.Body>
					<FormPasswordTextField className={"row my-2 px-2"} property={"oudeWachtwoord"} required error={formikProps.errors.oudeWachtwoord}
										   label={getString(properties.form.fields.oudeWachtwoord)}/>
					<FormPasswordTextField className={"row my-2 px-2"} property={"nieuweWachtwoord"} required error={formikProps.errors.nieuweWachtwoord}
										   label={getString(properties.form.fields.nieuweWachtwoord)}/>
					<FormPasswordTextField className={"row my-2 px-2"} property={"nieuweWachtwoordControle"} required error={formikProps.errors.nieuweWachtwoordControle}
										   label={getString(properties.form.fields.nieuweWachtwoordControle)}/>
				</Modal.Body>
				<Modal.Footer>
					<Button variant="secondary" onClick={props.onHide}>
						{getString(properties.form.button.close)}
					</Button>
					<Button variant="primary" type={"submit"}>
						{getString(properties.form.button.submit)}
					</Button>
				</Modal.Footer>
			</Form>}
		</Formik>
	</Modal>)
}

export default WachtwoordWijzigenModal
