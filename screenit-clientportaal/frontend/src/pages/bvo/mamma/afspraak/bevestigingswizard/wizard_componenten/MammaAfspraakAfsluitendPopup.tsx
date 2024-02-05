/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import BasePopup from "../../../../../../components/popup/BasePopup"
import properties from "./MammaAfspraakMakenWizardModuleProperties.json"
import styles from "./MammaAfspraakMakenWizardModuleStyles.module.scss"
import React from "react"
import Button from "../../../../../../components/input/Button"
import {ArrowType} from "../../../../../../components/vectors/ArrowIconComponent"
import {NavLink} from "react-bootstrap"
import {useNavigate} from "react-router-dom"
import {getBvoBaseUrl} from "../../../../../../utils/UrlUtil"
import {useSelectedBvo} from "../../../../../../utils/Hooks"
import {AfspraakBevestigingOpties} from "../../../../../../datatypes/mamma/AfspraakBevestigingOpties"
import {BevestigingsType} from "../../../../../../datatypes/BevestigingsType"

export type MammaAfspraakAfsluitendPopupProps = {
	afspraakBevestiging: AfspraakBevestigingOpties,
	onHuisartsControleren: () => void;
	children: React.ReactNode
}

const MammaAfspraakAfsluitendPopup = (props: MammaAfspraakAfsluitendPopupProps) => {
	const navigate = useNavigate()
	const bvo = useSelectedBvo()!

	return (<BasePopup
		title={properties.bevestiging.titel}
		description={maakOmschrijving(props.afspraakBevestiging)}
		children={
			<div>
				<div className={styles.bevestigenForm}>
					<Button label={properties.bevestiging.controleer_ha}
							onClick={() => {
								props.onHuisartsControleren()
							}}
							displayArrow={ArrowType.ARROW_RIGHT}
					/>

					<NavLink onClick={() => {
						navigate(getBvoBaseUrl(bvo))
					}}>{properties.bevestiging.sla_over}</NavLink>
				</div>
				{props.children}
			</div>}/>)
}

function maakOmschrijving(gekozenBevestiging: AfspraakBevestigingOpties): string {
	let tekst = properties.bevestiging.altijd
	if (BevestigingsType.MAIL === gekozenBevestiging.bevestigingsType) {
		tekst = tekst.concat(properties.bevestiging.mail)
	}
	if (BevestigingsType.BRIEF === gekozenBevestiging.bevestigingsType) {
		tekst = tekst.concat(properties.bevestiging.brief)
	}
	if (gekozenBevestiging.wilHerinneringsSms) {
		tekst = tekst.concat(properties.bevestiging.sms)
	}
	tekst = tekst.concat(properties.bevestiging.meenemen)
	return tekst
}

export default MammaAfspraakAfsluitendPopup
