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
import BasePopup from "../../../../components/popup/BasePopup"
import {Col, NavLink, Row} from "react-bootstrap"
import classNames from "classnames"
import {formatDateText, formatTime} from "../../../../utils/DateUtil"
import bvoStyles from "../../../../components/BvoStyle.module.scss"
import styles from "./ColonAfspraakMakenBevestigingsPopup.module.scss"
import {useSelectedBvo} from "../../../../utils/Hooks"
import {Bevolkingsonderzoek, BevolkingsonderzoekStyle} from "../../../../datatypes/Bevolkingsonderzoek"
import {getString} from "../../../../utils/TekstPropertyUtil"
import VerticalDividerComponent from "../../../../components/vectors/VerticalDividerComponent"
import {VrijSlotZonderKamer} from "./ColonAfspraakMakenPage"
import properties from "./ColonAfspraakMakenBevestigingsPopup.json"
import {afspraakVerplaatsen, nieuweAfspraak} from "../../../../api/ColonAfspraakMakenThunkAction"
import {useThunkDispatch} from "../../../../index"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import SubmitButton from "../../../../components/input/SubmitButton"
import {getBvoBaseUrl} from "../../../../utils/UrlUtil"
import {useNavigate} from "react-router-dom"

export type ColonAfspraakMakenBevestigingsPopupProps = {
	afspraak: VrijSlotZonderKamer
	heraanmelding: boolean
	onClose: () => void
}

const ColonAfspraakMakenBevestigingsPopup = (props: ColonAfspraakMakenBevestigingsPopupProps) => {
	const bvo = useSelectedBvo()
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()

	return (
		<BasePopup title={getString(properties.page.title)}
				   description={getString(properties.page.description)}
				   children={
					   <div>
						   <div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.afspraakDiv)}>
							   <VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={15}/>
							   <Row className={styles.afspraakGegevensRow}>
								   <Col sm={6}>
                                           <span
											   className={classNames(bvoStyles.bvoText)}>{getString(properties.appointment.description.datetime)}</span>
									   <span>{formatDateText(props.afspraak.startTijd)}</span>
									   <span>{getString(properties.appointment.values.time, [formatTime(props.afspraak.startTijd)])}</span>
								   </Col>
								   <Col sm={6} className={styles.locatieColumn}>
                                           <span
											   className={classNames(bvoStyles.bvoText)}>{getString(properties.appointment.description.location)}</span>
									   <span>{props.afspraak.ziekenhuis}</span>
									   <span>{props.afspraak.adres}</span>
									   <span>{props.afspraak.postcode + " " + props.afspraak.plaats}</span>
								   </Col>
							   </Row>
						   </div>
						   <div className={styles.buttons}>
							   <SubmitButton label={getString(properties.buttons.confirmationletter)}
											 displayArrow={ArrowType.ARROW_RIGHT}
											 onClick={() => {
												 if (props.heraanmelding) {
													 dispatch(nieuweAfspraak(props.afspraak, props.onClose)).then(() => navigate(getBvoBaseUrl(Bevolkingsonderzoek.COLON)))
												 } else {
													 dispatch(afspraakVerplaatsen(props.afspraak, props.onClose)).then(() => navigate(getBvoBaseUrl(Bevolkingsonderzoek.COLON)))
												 }
											 }}/>
							   <NavLink onClick={props.onClose} className={styles.andereOptie}>
								   {getString(properties.buttons.differentappointment)}</NavLink>
						   </div>
					   </div>
				   }/>
	)
}

export default ColonAfspraakMakenBevestigingsPopup
