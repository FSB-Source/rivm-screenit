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
import styles from "./GegevensWijzigenFormComponent.module.scss"
import properties from "./GegevensWijzigenFormComponent.json"
import validatieProperties from "./../../util/ValidatieUtil.json"
import {useAppSelector, useAppThunkDispatch} from "../../index"
import {HuisartsDto} from "../../state/datatypes/dto/HuisartsDto"
import {Field, Form, Formik} from "formik"
import {Button, Col, Row} from "react-bootstrap"
import {getString} from "../../util/TekstPropertyUtil"
import FormTextField from "../form/text/FormTextField"
import FormWoonplaatsSelectField from "../form/adres/FormWoonplaatsSelectField"
import BaseFormField from "../form/BaseFormField"
import FormHuisnummerToevoegingField from "../form/adres/FormHuisnummerToevoegingField"
import * as Yup from "yup"
import {loadingThunkAction} from "../../api/LoadingThunkAction"
import {controleerHuisarts, saveHuisarts} from "../../api/HuisartsThunkAction"
import React, {useCallback, useEffect, useState} from "react"
import LocatieTabel from "../locatie/tabel/LocatieTabel"
import LocatieToevoegenModal from "../locatie/toevoegen/LocatieToevoegenModal"
import {AuthenticationScope} from "../../state/datatypes/enums/AuthenticationScope"
import {LocatieStatus} from "../../state/datatypes/dto/LocatieDto"
import {createActionPushToast} from "../../state/ToastsState"
import {ToastType} from "../../state/datatypes/Toast"
import {afmelden} from "../../api/AfmeldenThunkAction"
import WachtwoordWijzigenModal from "./wachtwoord/WachtwoordWijzigenModal"
import FormSelectField from "../form/select/FormSelectField"
import ScreenitBackend from "../../util/Backend"
import {postcodeValidatie, wachtwoordValidatie} from "../../util/ValidatieUtil"
import {Recht} from "../../state/datatypes/enums/Recht"
import {fetchCurrentUser} from "../../api/CurrentUserThunkAction"
import {useNavigate} from "react-router"

const GegevensWijzigenFormComponent = () => {
	const oauth = useAppSelector((state => state.oauth))
	const huisarts = useAppSelector((state => state.huisarts))
	const locaties = useAppSelector((state => state.locaties.values))
	const user = useAppSelector((state => state.user))

	const isRegistreren = oauth?.scope === AuthenticationScope.REGISTREREN

	const [controleren, setControleren] = useState<boolean>(false)
	const [nieuweLocatieToevoegen, setNieuweLocatieToevoegen] = useState<boolean>(false)
	const [wachtwoordWijzigen, setWachtwoordWijzigen] = useState<boolean>(false)

	const navigate = useNavigate()
	const dispatch = useAppThunkDispatch()

	useEffect(() => {
		if (user && user.rollen.includes(Recht.ROLE_OVEREENKOMST)) {
			dispatch(createActionPushToast({type: ToastType.INFO, message: getString(properties.message.overeenkomstGewijzigd)}))
		} else if (user && user.rollen.includes(Recht.ROLE_REGISTEREN)) {
			dispatch(createActionPushToast({type: ToastType.INFO, message: getString(properties.message.registrerenVerplicht)}))
		}
	}, [dispatch, user])

	const validationSchema = Yup.object().shape(
		{
			...isRegistreren && {
				wachtwoord: wachtwoordValidatie(getString(properties.agbNaw.fields.wachtwoord))
					.required(getString(validatieProperties.required)),
				wachtwoordControle: Yup.string()
					.oneOf([Yup.ref("wachtwoord"), undefined], getString(validatieProperties.wachtwoordControle))
					.required(getString(validatieProperties.required)),
			},
			email: Yup.string()
				.nullable()
				.email(getString(validatieProperties.email))
				.required(getString(validatieProperties.required)),
			extraEmails: Yup.string()
				.nullable()
				.email(getString(validatieProperties.email)),
			username: Yup.string()
				.min(6, getString(validatieProperties.minLength, [getString(properties.agbNaw.fields.username)])),
			achternaam: Yup.string()
				.required(getString(validatieProperties.required)),
			postadres: Yup.object({
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
		})

	const openOvereenkomst = useCallback(() => {
		ScreenitBackend.request({
			url: "/overeenkomst",
			method: "GET",
			responseType: "arraybuffer",
		}).then((response => {
			const downloadUrl = URL.createObjectURL(new Blob([new Uint8Array([0xEF, 0xBB, 0xBF]), response.data], {type: "octet/stream;charset=UTF-8"}))
			const downloadElement = document.createElement("a")
			if (typeof downloadElement.download === "undefined") {
				window.location.href = downloadUrl
			} else {
				downloadElement.href = downloadUrl
				downloadElement.download = "Zakelijke voorwaarden huisartsen.pdf"
				document.body.appendChild(downloadElement)
				downloadElement.click()
			}
		}))
	}, [])

	return <div className={styles.style}>
		{huisarts && <Formik<HuisartsDto>
			initialValues={{
				huisartsportaalId: huisarts.huisartsportaalId || 0,
				agbcode: huisarts.agbcode || "",
				email: huisarts.email || "",
				aanhef: huisarts.aanhef || "",
				achternaam: huisarts.achternaam || "",
				tussenvoegsel: huisarts.tussenvoegsel || "",
				voorletters: huisarts.voorletters || "",
				telefoon: huisarts.telefoon || "",
				username: huisarts.username || "",
				wachtwoord: huisarts.wachtwoord || "",
				wachtwoordControle: huisarts.wachtwoordControle || "",
				overeenkomst: huisarts.overeenkomst || !isRegistreren,
				extraEmails: huisarts.extraEmails || "",
				overeenkomstGeaccordeerdDatum: huisarts.overeenkomstGeaccordeerdDatum || "",
				postadres: {
					huisartsportaalId: huisarts.postadres?.huisartsportaalId || 0,
					straat: huisarts.postadres?.straat || "",
					huisnummer: huisarts.postadres?.huisnummer || "" as unknown as number,
					huisnummertoevoeging: huisarts.postadres?.huisnummertoevoeging || "",
					postcode: huisarts.postadres?.postcode || "",
					woonplaats: {
						huisartsportaalId: huisarts.postadres?.woonplaats?.huisartsportaalId || 0,
						naam: huisarts.postadres?.woonplaats?.naam || "",
						gemeente: huisarts.postadres?.woonplaats?.gemeente || "",
					},
				},

			}}
			validationSchema={validationSchema}
			onSubmit={(values) => {
				if (!controleren && isRegistreren) {
					dispatch(loadingThunkAction(controleerHuisarts(values))).then(() => {
						dispatch(createActionPushToast({type: ToastType.INFO, message: getString(properties.message.controleren)}))
						setControleren(true)
					})
					window.scrollTo(0, 0)
				} else {
					dispatch(loadingThunkAction(saveHuisarts(values))).then(() => {
						if (isRegistreren) {
							if (locaties?.locaties && !!locaties?.locaties.find(locatie => locatie.status === LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD)) {
								dispatch(afmelden(oauth)).then(() => {
									dispatch(createActionPushToast({type: ToastType.INFO, message: getString(properties.message.zorgmailVerificatie)}))
								})
							}
						} else {
							dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.message.opgeslagen)}))
							if (user && user.rollen.includes(Recht.ROLE_OVEREENKOMST)) {
								dispatch(loadingThunkAction(fetchCurrentUser())).then(() => navigate("/"))
							}
						}
					})
				}
			}}
		>
			{(props) => (
				<Form>
					<LocatieToevoegenModal show={!!huisarts && nieuweLocatieToevoegen} onHide={() => setNieuweLocatieToevoegen(false)} nawGegevens={props.values.postadres}/>
					<WachtwoordWijzigenModal show={wachtwoordWijzigen} onHide={() => setWachtwoordWijzigen(false)}/>
					<h3 className={"mt-4"}>{properties.agbNaw.title}</h3>
					<Row>
						<Col md={6}>
							<FormTextField className={"row my-3"} property={"agbcode"} disabled alignRight required
										   label={getString(properties.agbNaw.fields.agbcode)}/>
							<FormTextField className={"row my-3"} property={"email"} disabled={controleren} error={props.errors.email} alignRight required
										   label={getString(properties.agbNaw.fields.email)}/>
							<FormTextField className={"row my-3"} property={"extraEmails"} disabled={controleren} error={props.errors.extraEmails} alignRight
										   label={getString(properties.agbNaw.fields.extraEmails)}/>
						</Col>
						<Col md={6}>
							<FormTextField className={"row my-3"} property={"username"} disabled={controleren || !isRegistreren} error={props.errors.username} alignRight required
										   label={getString(properties.agbNaw.fields.username)}/>
							{isRegistreren ? <>
								<FormTextField className={"row my-3"} property={"wachtwoord"} disabled={controleren} type={"password"}
											   error={props.errors.wachtwoord} alignRight
											   required label={getString(properties.agbNaw.fields.wachtwoord)}/>
								<FormTextField className={"row my-3"} property={"wachtwoordControle"} disabled={controleren} type={"password"}
											   error={props.errors.wachtwoordControle}
											   alignRight required label={getString(properties.agbNaw.fields.wachtwoordControle)}/>
							</> : <BaseFormField className={"row my-3"} property={"nieuwWachtwoord"} alignRight label={getString(properties.agbNaw.fields.wachtwoord)}>
								<Button variant={"secondary"} onClick={() => setWachtwoordWijzigen(true)}>Wachtwoord wijzigen</Button>
							</BaseFormField>}
						</Col>
					</Row>
					<hr/>
					<Row>
						<Col md={6}>
							<FormSelectField<string> className={"row my-3"} property={"aanhef"} disabled={controleren} alignRight clearable
													 label={getString(properties.agbNaw.fields.aanhef)}
													 options={[{value: "", label: ""}, {value: "Dhr.", label: "Dhr."}, {value: "Mevr.", label: "Mevr."}]}
													 value={props.values.aanhef}
													 setValue={(value) => props.setFieldValue("aanhef", value)}/>
							<FormTextField className={"row my-3"} property={"achternaam"} disabled={controleren} maxLength={100} alignRight required
										   label={getString(properties.agbNaw.fields.achternaam)}/>
							<FormTextField className={"row my-3"} property={"tussenvoegsel"} disabled={controleren} maxLength={20} alignRight
										   label={getString(properties.agbNaw.fields.tussenvoegsel)}/>
							<FormTextField className={"row my-3"} property={"voorletters"} disabled={controleren} maxLength={20} alignRight
										   label={getString(properties.agbNaw.fields.voorletters)}/>
							<FormTextField className={"row my-3"} property={"telefoon"} disabled={controleren} maxLength={25} alignRight
										   label={getString(properties.agbNaw.fields.telefoon)}/>
						</Col>
						<Col md={6}>
							<FormTextField className={"row my-3"} property={"postadres.straat"} disabled={controleren} error={props.errors.postadres?.straat} alignRight required
										   label={getString(properties.agbNaw.fields.postadres.straat)}/>

							<FormHuisnummerToevoegingField className={"row my-3"} required disabled={controleren} alignRight property={"postadres.huisnummer"}
														   toevoegingProperty={"postadres.huisnummertoevoeging"} error={props.errors.postadres?.huisnummer}
														   label={getString(properties.agbNaw.fields.postadres.huisnummertoevoeging)} huisnummerPlaceholder={"Huisnummer"}
														   toevoegingPlaceholder={"Toevoeging"}/>

							<FormTextField className={"row my-3"} property={"postadres.postcode"} disabled={controleren} error={props.errors.postadres?.postcode} alignRight
										   required
										   label={getString(properties.agbNaw.fields.postadres.postcode)} maxLength={6}/>

							<BaseFormField className={"row my-3"} property={"postadres.woonplaats"}
										   error={props.errors.postadres?.woonplaats && String(props.errors.postadres?.woonplaats)} alignRight required
										   label={getString(properties.agbNaw.fields.postadres.woonplaats)}>
								<Field name={"postadres.woonplaats"} property={"postadres.woonplaats"} disabled={controleren} selectedValue={props.values.postadres?.woonplaats}
									   component={FormWoonplaatsSelectField}/>
							</BaseFormField>
						</Col>
					</Row>
					<hr/>
					<h3>{properties.locaties.title}</h3>
					<LocatieTabel nawGegevens={props.values.postadres}/>
					{(!controleren && locaties.aantalLocaties === 0) && <Button className={styles.btnLocatieToevoegen} variant={"primary"}
																				onClick={() => setNieuweLocatieToevoegen(true)}>{getString(properties.locaties.nieuweLocatie)}</Button>}
					<div className={styles.zakelijkeVoorwaardenBlock}>
						<hr/>
						<h3>{properties.zakelijkeVoorwaarden.title}</h3>
						<Field className={"mx-2 d-inline-block"} id={"overeenkomst"} name={"overeenkomst"} type={"checkbox"} disabled={controleren} required/>
						<div className={"d-inline-block"}>
							<span className={"fw-bold"}>{getString(properties.zakelijkeVoorwaarden.akkoord)} </span>
							<span className={styles.overeenkomstLink} onClick={openOvereenkomst}>{getString(properties.zakelijkeVoorwaarden.zakelijkeVoorwaarden)}</span>
						</div>
						<hr className={"mb-4"}/>
						{controleren && <Button className={"w-100 my-1"} onClick={() => setControleren(false)} variant={"secondary"}>{getString(properties.form.terug)}</Button>}
						<Button variant={"primary"} className={"w-100 my-1"}
								type={"submit"}>{getString(controleren ? properties.form.voltooien : isRegistreren ? properties.form.controleren : properties.form.opslaan)}</Button>
					</div>
				</Form>)}
		</Formik>}
	</div>
}

export default GegevensWijzigenFormComponent
