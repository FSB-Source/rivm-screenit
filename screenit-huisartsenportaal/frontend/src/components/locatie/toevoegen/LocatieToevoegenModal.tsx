/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import styles from "./LocatieToevoegenModal.module.scss"
import {Button, Col, Modal, Row} from "react-bootstrap"
import {getString} from "../../../util/TekstPropertyUtil"
import properties from "./LocatieToevoegenModal.json"
import validatieProperties from "../../../util/ValidatieUtil.json"
import {LocatieDto, LocatieStatus} from "../../../state/datatypes/dto/LocatieDto"
import {Field, Form, Formik} from "formik"
import * as Yup from "yup"
import FormTextField from "../../form/text/FormTextField"
import BaseFormField from "../../form/BaseFormField"
import {useState} from "react"
import classNames from "classnames"
import FormHuisnummerToevoegingField from "../../form/adres/FormHuisnummerToevoegingField"
import FormWoonplaatsSelectField from "../../form/adres/FormWoonplaatsSelectField"
import {AdresDto} from "../../../state/datatypes/dto/AdresDto"
import {useAppThunkDispatch} from "../../../index"
import {loadingThunkAction} from "../../../api/LoadingThunkAction"
import {putLocatie} from "../../../api/LocatieThunkAction"
import {createActionPushToast} from "../../../state/ToastsState"
import {ToastType} from "../../../state/datatypes/Toast"
import {ibanValidatie, postcodeValidatie, zorgmailKlantnummerValidatie} from "../../../util/ValidatieUtil"

export interface LocatieToevoegenModalProps {
	initialValues?: LocatieDto;
	show: boolean;
	onHide: () => void;
	nawGegevens?: AdresDto;
}

const LocatieVerwijderenModal = (props: LocatieToevoegenModalProps) => {
	const [gebruikNAW, setGebruikNAW] = useState<boolean>(false)
	const dispatch = useAppThunkDispatch()

	return <Modal size={"xl"} show={props.show} onHide={() => {
		props.onHide()
		setGebruikNAW(false)
	}} className={styles.style}>
		<Modal.Header closeButton>
			<Modal.Title>{getString(props.initialValues ? properties.title.wijzigen : properties.title.toevoegen)}</Modal.Title>
		</Modal.Header>
		<Formik<LocatieDto>
			initialValues={{
				huisartsportaalId: props.initialValues?.huisartsportaalId,
				naam: props.initialValues?.naam || "",
				zorgmailklantnummer: props.initialValues?.zorgmailklantnummer || "",
				iban: props.initialValues?.iban || "",
				ibanTenaamstelling: props.initialValues?.ibanTenaamstelling || "",
				status: props.initialValues?.status || LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD,
				locatieAdres: {
					huisartsportaalId: props.initialValues?.locatieAdres?.huisartsportaalId,
					straat: props.initialValues?.locatieAdres?.straat || "",
					huisnummer: props.initialValues?.locatieAdres?.huisnummer || "" as unknown as number,
					huisnummertoevoeging: props.initialValues?.locatieAdres?.huisnummertoevoeging || "",
					postcode: props.initialValues?.locatieAdres?.postcode || "",
					woonplaats: {
						huisartsportaalId: props.initialValues?.locatieAdres?.woonplaats?.huisartsportaalId,
						naam: props.initialValues?.locatieAdres?.woonplaats?.naam || "",
						gemeente: props.initialValues?.locatieAdres?.woonplaats?.gemeente || "",
					},
				},
			}}
			validationSchema={Yup.object().shape({
				naam: Yup.string()
					.required(getString(validatieProperties.required)),
				zorgmailklantnummer: zorgmailKlantnummerValidatie(getString(properties.form.fields.zorgmailklantnummer))
					.required(getString(validatieProperties.required)),
				iban: ibanValidatie()
					.required(getString(validatieProperties.required)),
				ibanTenaamstelling: Yup.string()
					.required(getString(validatieProperties.required)),
				locatieAdres: Yup.object({
					straat: Yup.string()
						.required(getString(validatieProperties.required)),
					huisnummer: Yup.number()
						.min(0, getString(validatieProperties.min))
						.required(getString(validatieProperties.required)),
					postcode: postcodeValidatie()
						.required(getString(validatieProperties.required)),
					woonplaats: Yup.object()
						.nullable()
						.required(getString(validatieProperties.required)),
				}),
			})}
			onSubmit={(values) => {
				dispatch(loadingThunkAction(putLocatie(values))).then((locatie) => {
					if (props.initialValues && locatie.status === LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD) {
						dispatch(createActionPushToast({type: ToastType.WARN, message: getString(properties.message.zorgmailVerificatie)}))
					} else {
						dispatch(createActionPushToast({type: ToastType.SUCCESS, message: "De locatie is succesvol opgelagen"}))
					}
					locatie && props.onHide()
				})
			}}
		>
			{formikProps => {
				return <Form>
					<Modal.Body>
						<Row>
							<Col md={6}>
								<FormTextField className={"row my-2"} property={"naam"} alignRight required error={formikProps.errors.naam}
											   label={getString(properties.form.fields.naam)}/>
								<FormTextField className={"row my-2"} property={"zorgmailklantnummer"} alignRight required error={formikProps.errors.zorgmailklantnummer}
											   label={getString(properties.form.fields.zorgmailklantnummer)}
											   placeholder={getString(properties.form.placeholder.zorgmailklantnummer)}/>
								<FormTextField className={"row my-2"} property={"iban"} alignRight required error={formikProps.errors.iban}
											   label={getString(properties.form.fields.iban)}
											   placeholder={getString(properties.form.placeholder.iban)}/>
								<FormTextField className={"row my-2"} property={"ibanTenaamstelling"} alignRight required error={formikProps.errors.ibanTenaamstelling}
											   label={getString(properties.form.fields.ibanTenaamstelling)}/>

							</Col>
							<Col md={6}>
								{props.nawGegevens && <BaseFormField className={classNames(styles.gelijkAanNAW, "row my-2")} alignRight property={"gelijkAanNaw"}
																	 label={getString(properties.form.fields.gelijkAanNAW)}>
									<input type={"checkbox"} checked={gebruikNAW} onChange={(e) => {
										setGebruikNAW(e.target.checked)
										formikProps.setFieldValue("locatieAdres.straat", props.nawGegevens!.straat)
										formikProps.setFieldValue("locatieAdres.huisnummer", props.nawGegevens!.huisnummer)
										formikProps.setFieldValue("locatieAdres.huisnummertoevoeging", props.nawGegevens!.huisnummertoevoeging)
										formikProps.setFieldValue("locatieAdres.postcode", props.nawGegevens!.postcode)
										formikProps.setFieldValue("locatieAdres.woonplaats", props.nawGegevens!.woonplaats)
										dispatch(loadingThunkAction(() => {
											return new Promise(resolve => setTimeout(resolve, 200)).then(formikProps.validateForm)
										}))
									}
									}/>
								</BaseFormField>}
								<FormTextField className={"row my-2"} property={"locatieAdres.straat"} alignRight required error={formikProps.errors.locatieAdres?.straat}
											   label={getString(properties.form.fields.locatieAdres.straat)} disabled={gebruikNAW}/>
								<FormHuisnummerToevoegingField className={"row my-2"} required alignRight property={"locatieAdres.huisnummer"}
															   toevoegingProperty={"locatieAdres.huisnummertoevoeging"} error={formikProps.errors.locatieAdres?.huisnummer}
															   label={getString(properties.form.fields.locatieAdres.huisnummertoevoeging)} huisnummerPlaceholder={"Huisnummer"}
															   toevoegingPlaceholder={"Toevoeging"} disabled={gebruikNAW}/>

								<FormTextField className={"row my-3"} property={"locatieAdres.postcode"} error={formikProps.errors.locatieAdres?.postcode} alignRight required
											   label={getString(properties.form.fields.locatieAdres.postcode)} maxLength={6} disabled={gebruikNAW}/>

								<BaseFormField className={"row my-3"} property={"locatieAdres.woonplaats"}
											   error={formikProps.errors.locatieAdres?.woonplaats && String(formikProps.errors.locatieAdres?.woonplaats)} alignRight required
											   label={getString(properties.form.fields.locatieAdres.woonplaats)}>
									{}
									<Field name={"locatieAdres.woonplaats"} property={"locatieAdres.woonplaats"} disabled={gebruikNAW}
										   selectedValue={formikProps.values.locatieAdres?.woonplaats}
										   component={FormWoonplaatsSelectField}/>
								</BaseFormField>
							</Col>
						</Row>
					</Modal.Body>
					<Modal.Footer>
						<Button variant="secondary" onClick={props.onHide}>
							{getString(properties.form.button.close)}
						</Button>
						<Button variant="primary" type={"submit"}>
							{getString(properties.form.button.submit)}
						</Button>
					</Modal.Footer>
				</Form>
			}}
		</Formik>
	</Modal>
}

export default LocatieVerwijderenModal
