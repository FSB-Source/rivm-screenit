/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import BasePopup from "../../../../../../components/popup/BasePopup"
import properties from "./MammaAfspraakMakenWizardModuleProperties.json"
import styles from "./MammaAfspraakMakenWizardModuleStyles.module.scss"
import ScreenitTextfield from "../../../../../../components/input/ScreenitTextfield"
import Button from "../../../../../../components/input/Button"
import {NavLink} from "react-bootstrap"
import {ArrowType} from "../../../../../../components/vectors/ArrowIconComponent"
import * as Yup from "yup"
import {getString} from "../../../../../../utils/TekstPropertyUtil"
import {isMobielnummerValid} from "../../../../../../validators/TelefoonnummerValidator"
import {Formik} from "formik"
import {AfspraakBevestigingOpties} from "../../../../../../datatypes/mamma/AfspraakBevestigingOpties"

export type MammaHerinneringSmsPopupProps = {
	afspraakBevestiging: AfspraakBevestigingOpties,
	children: React.ReactNode
	onVolgende: () => void;
}

const MammaHerinneringSmsPopup = (props: MammaHerinneringSmsPopupProps) => {
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		clientNieuwMobielNummer: Yup.string().required(getString(properties.sms.sms_validatie.niet_aanwezig))
			.test("telefoonnummerValidatie", getString(properties.sms.sms_validatie.fout), function (value) {
				return isMobielnummerValid(value)
			}),
	})

	return (<BasePopup
		title={properties.sms.titel}
		description={properties.sms.controleer_mobiel_tekst}
		children={
			<div>
				<h3>{properties.sms.boven_invoerveld}</h3>
				<div className={styles.bevestigenForm}>
					<Formik
						initialValues={props.afspraakBevestiging}
						validationSchema={validatieSchema}
						onSubmit={() => {
							props.afspraakBevestiging.wilHerinneringsSms = true
							props.onVolgende()
						}}>
						{formikProps => (
							<><ScreenitTextfield
								name={"mobielnummer"}
								placeholder={"Mobiel nummer"}
								value={formikProps.values.clientNieuwMobielNummer}
								invalidMessage={formikProps.errors.clientNieuwMobielNummer}
								onChange={value => {
									formikProps.setFieldValue("clientNieuwMobielNummer", value)
								}}/>
								<Button label={properties.sms.sms_knop_tekst}
										disableButton={formikProps.isSubmitting}
										onClick={() => {
											props.afspraakBevestiging.clientNieuwMobielNummer = formikProps.values.clientNieuwMobielNummer
											formikProps.handleSubmit()
										}}
										displayArrow={ArrowType.ARROW_RIGHT}/></>)}
					</Formik>
					<NavLink onClick={props.onVolgende}>{properties.sms.geen_sms_tekst}</NavLink>
				</div>
				{props.children}
			</div>}/>)
}

export default MammaHerinneringSmsPopup
