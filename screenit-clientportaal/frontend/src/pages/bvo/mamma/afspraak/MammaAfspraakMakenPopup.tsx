/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import React from "react"
import {KandidaatAfspraak} from "../../../../datatypes/mamma/KandidaatAfspraak"
import BasePopup from "../../../../components/popup/BasePopup"
import {Col, NavLink, Row} from "react-bootstrap"
import classNames from "classnames"
import {formatDateText, formatTime} from "../../../../utils/DateUtil"
import bvoStyles from "../../../../components/BvoStyle.module.scss"
import styles from "./MammaAfspraakMakenPopup.module.scss"
import {useSelectedBvo} from "../../../../utils/Hooks"
import {BevolkingsonderzoekStyle} from "../../../../datatypes/Bevolkingsonderzoek"
import Button from "../../../../components/input/Button"
import {maakAfspraak} from "../../../../api/MammaAfspraakMakenThunkAction"
import {useThunkDispatch} from "../../../../index"
import {getString} from "../../../../utils/TekstPropertyUtil"
import VerticalDividerComponent from "../../../../components/vectors/VerticalDividerComponent"
import properties from "./MammaAfspraakMakenPopup.json"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {Formik} from "formik"
import {Checkbox} from "@material-ui/core"
import {getBvoBaseUrl} from "../../../../utils/UrlUtil"
import {useNavigate} from "react-router-dom"

export type MammaAfspraakMakenPopupProps = {
	afspraak: KandidaatAfspraak,
	isBevestigingsPopup: boolean,
	onFailure?: () => void,
	onClose: () => void
}

type MammaAfspraakMakenPopupForm = {
	bevestigingsBrief: boolean;
}

const MammaAfspraakMakenPopup = (props: MammaAfspraakMakenPopupProps) => {
	const bvo = useSelectedBvo()!
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const kandidaatAfspraak = props.afspraak

	const afspraakMaken = (afspraak: KandidaatAfspraak) => {
		dispatch(maakAfspraak(bvo, afspraak)).then(
			() => {
				navigate(getBvoBaseUrl(bvo))
				props.onClose()
			},
		).catch((error) => {
			if (error.response.data === "tijd.niet.beschikbaar") {
				props.onFailure && props.onFailure()
			}
		})
	}

	const initialValues: MammaAfspraakMakenPopupForm = {
		bevestigingsBrief: props.afspraak.bevestigingsBrief,
	}

	return (
		<BasePopup title={props.isBevestigingsPopup ? getString(properties.bevestiging.title) : getString(properties.failure.title)}
				   description={props.isBevestigingsPopup ? getString(properties.bevestiging.description) : getString(properties.failure.description)}
				   children={
					   <div>
						   <div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.afspraakDiv)}>
							   <VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={15}/>
							   <Row className={styles.afspraakGegevensRow}>
								   <Col sm={6}>
									   <span className={classNames(bvoStyles.bvoText)}>{getString(properties.headers.datumtijd)}</span>
									   <span>{formatDateText(props.afspraak.datumTijd)}</span>
									   <span>{formatTime(props.afspraak.datumTijd) + " uur"}</span>
								   </Col>
								   <Col sm={6} className={styles.locatieColumn}>
									   <span className={classNames(bvoStyles.bvoText)}>{getString(properties.headers.locatie)}</span>
									   <span>{props.afspraak.adres}</span>
									   <span>{props.afspraak.postcode + " " + props.afspraak.plaats}</span>
								   </Col>
							   </Row>
						   </div>
						   <div className={styles.bevestigenForm}>
							   {props.isBevestigingsPopup &&
								   <div>
									   <Formik<MammaAfspraakMakenPopupForm> initialValues={initialValues}
																			onSubmit={(values) => {
																				kandidaatAfspraak.bevestigingsBrief = values.bevestigingsBrief
																				afspraakMaken(kandidaatAfspraak)
																			}}>
										   {formikProps => (
											   <div>
												   <div className={classNames(styles.bevestigingsBrief, !props.afspraak.toonBevestigingsBriefOptie && styles.hide)}>
													   <Checkbox id={"bevestigingsBrief"}
																 defaultChecked={initialValues.bevestigingsBrief}
																 onChange={(event) => formikProps.setFieldValue("bevestigingsBrief", event.target.checked)}/>
													   <label htmlFor={"bevestigingsBrief"}>{getString(properties.checkbox.bevestigingsbrief)}</label>
												   </div>
												   <Button label={getString(properties.button.bevestigen)}
														   disableButton={formikProps.isSubmitting}
														   displayArrow={ArrowType.ARROW_RIGHT}
														   onClick={() => {
															   formikProps.handleSubmit()
														   }}/>
											   </div>)}
									   </Formik>
									   <NavLink onClick={props.onClose} className={styles.andereOptie}>
										   {getString(properties.button.andere_afspraak)}</NavLink>
								   </div>}
							   {!props.isBevestigingsPopup &&
								   <Button label={getString(properties.button.andere_afspraak)}
										   onClick={props.onClose}
										   displayArrow={ArrowType.ARROW_RIGHT}/>}
						   </div>
					   </div>
				   }
		/>)
}

export default MammaAfspraakMakenPopup
