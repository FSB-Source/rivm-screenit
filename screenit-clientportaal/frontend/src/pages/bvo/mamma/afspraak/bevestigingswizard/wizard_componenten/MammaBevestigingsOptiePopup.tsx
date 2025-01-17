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
import {ArrowType} from "../../../../../../components/vectors/ArrowIconComponent"
import {NavLink} from "react-bootstrap"
import {AfspraakBevestigingOpties} from "../../../../../../datatypes/mamma/AfspraakBevestigingOpties"
import * as Yup from "yup"
import {isEmailadresValid} from "../../../../../../validators/EmailValidator"
import {getString} from "../../../../../../utils/TekstPropertyUtil"
import {Formik} from "formik"
import {BevestigingsType} from "../../../../../../datatypes/BevestigingsType"

export type MammaBevestigingsOptiePopupProps = {
	afspraakBevestiging: AfspraakBevestigingOpties,
	children: React.ReactNode
	onVolgende: () => void;
}

const MammaBevestigingsOptiePopup = (props: MammaBevestigingsOptiePopupProps) => {

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		clientNieuwEmailAdres: Yup.string().required(getString(properties.mail.email_validatie.niet_aanwezig))
			.test("mailValidatie", getString(properties.mail.email_validatie.fout), function (value) {
				return isEmailadresValid(value)
			})
			.max(100, getString(properties.mail.email_validatie.te_lang)),
	})

	return (<BasePopup
		title={properties.mail.titel}
		description={properties.mail.controleer_email_tekst}
		children={
			<div>
				<h3>{properties.mail.boven_invoerveld}</h3>
				<div className={styles.bevestigenForm}>
					<Formik initialValues={props.afspraakBevestiging}
							validationSchema={validatieSchema}
							onSubmit={() => {
								props.afspraakBevestiging.bevestigingsType = BevestigingsType.MAIL
								props.onVolgende()
							}}>
						{formikProps => (
							<><ScreenitTextfield name={"mail"}
												 placeholder={"E-mailadres"}
												 value={formikProps.values.clientNieuwEmailAdres}
												 invalidMessage={formikProps.errors.clientNieuwEmailAdres}
												 onChange={value => {
													 formikProps.setFieldValue("clientNieuwEmailAdres", value)
												 }}/>
								<Button label={properties.mail.mail_knop_tekst}
										disableButton={formikProps.isSubmitting}
										onClick={() => {
											props.afspraakBevestiging.clientNieuwEmailAdres = formikProps.values.clientNieuwEmailAdres
											formikProps.handleSubmit()
										}}
										displayArrow={ArrowType.ARROW_RIGHT}/></>)}
					</Formik>
					{props.afspraakBevestiging.toonBriefOptie &&
						<NavLink onClick={() => {
							props.afspraakBevestiging.bevestigingsType = BevestigingsType.BRIEF
							props.onVolgende()
						}}>{properties.mail.brief_link_tekst}</NavLink>}

					<NavLink onClick={props.onVolgende}>{properties.mail.geen_bevestiging_tekst}</NavLink>
				</div>
				{props.children}
			</div>}/>)
}

export default MammaBevestigingsOptiePopup
