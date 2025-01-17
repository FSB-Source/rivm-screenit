/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {Form, Formik} from "formik"
import {Button, Modal} from "react-bootstrap"
import {LocatieVerificatieDto} from "../../../state/datatypes/dto/LocatieVerificatieDto"
import properties from "./LocatieVerificatieModal.json"
import validatieProperties from "../../../util/ValidatieUtil.json"
import * as Yup from "yup"
import {getString} from "../../../util/TekstPropertyUtil"
import {useAppThunkDispatch} from "../../../index"
import {createActionPushToast} from "../../../state/ToastsState"
import {ToastType} from "../../../state/datatypes/Toast"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import FormTextField from "../../form/text/FormTextField"
import {herzendVerificatieCode, verifieerLocatie} from "../../../api/LocatieThunkAction"

export interface LocatieVerificatieModalProps {
	locatie?: LocatieVerificatieDto;
	hide: () => void;
}

const LocatieVerificatieModal = (props: LocatieVerificatieModalProps) => {
	const dispatch = useAppThunkDispatch()

	return (<Modal size={"lg"} show={!!props.locatie} onHide={props.hide}>
		<Modal.Header closeButton>
			<Modal.Title>{getString(properties.title)}</Modal.Title>
		</Modal.Header>
		<Formik<LocatieVerificatieDto>
			initialValues={{
				...props.locatie!,
				verificatieCode: undefined,
			}}
			validationSchema={Yup.object().shape({
				verificatieCode: Yup.string()
					.matches(/^[0-9]*$/, getString(validatieProperties.type))
					.min(4, getString(validatieProperties.minLength, [getString(properties.form.label.verificatieCode)]))
					.max(4, getString(validatieProperties.maxLength, [getString(properties.form.label.verificatieCode)]))
					.required(getString(validatieProperties.required)),
			})}
			onSubmit={locatie => {
				dispatch(loadingThunkAction(verifieerLocatie(locatie))).then(() => {
					dispatch(createActionPushToast({type: ToastType.INFO, message: getString(properties.verificatie.success)}))
				}).catch(() => {
					dispatch(createActionPushToast({type: ToastType.ERROR, message: getString(properties.verificatie.error)}))
				})
			}}
		>
			{formikProps => <Form>
				<Modal.Body>
					<p>{getString(properties.form.description)}</p>
					<FormTextField className={"row my-2 px-2"} property={"locatieNaam"} disabled label={getString(properties.form.label.locatienaam)}/>
					<FormTextField className={"row my-2 px-2"} property={"zorgmailKlantnummer"} disabled label={getString(properties.form.label.zorgmailKlantnummer)}/>
					<FormTextField className={"row my-2 px-2"} property={"verificatieCode"} required error={formikProps.errors.verificatieCode} maxLength={4}
								   label={getString(properties.form.label.verificatieCode)}/>
				</Modal.Body>
				<Modal.Footer>
					<Button variant="secondary" onClick={() => {
						dispatch(loadingThunkAction(herzendVerificatieCode(props.locatie!))).then(() => {
							dispatch(createActionPushToast({type: ToastType.INFO, message: getString(properties.resend.success)}))
						}).catch(() => {
							dispatch(createActionPushToast({type: ToastType.ERROR, message: getString(properties.resend.error)}))
						})
					}}>
						{getString(properties.form.button.resend)}
					</Button>
					<Button variant="secondary" onClick={props.hide}>
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

export default LocatieVerificatieModal
