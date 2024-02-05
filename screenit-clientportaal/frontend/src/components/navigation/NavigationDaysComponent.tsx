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
import styles from "../../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage.module.scss"
import Button from "../input/Button"
import {formatDateText} from "../../utils/DateUtil"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "../../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage.json"
import {ArrowType} from "../vectors/ArrowIconComponent"
import React from "react"
import {Col, Row} from "react-bootstrap"
import classNames from "classnames"

export type NavigationDaysComponentProps = {
	geselecteerdeDatum?: Date,
	terugKnopIsZichtbaar?: boolean
	vooruitKnopIsZichtbaar?: boolean
	onClickBack: () => void,
	onClickForward: () => void
}

const NavigationDaysComponent = (props: NavigationDaysComponentProps) => {
	return (
		<Row className={classNames(styles.navigationButtons, "text-center")}>
			<Col xs={2} className={styles.noPadding}>
				{props.terugKnopIsZichtbaar &&
				<Button className={styles.left}
						lightStyle={true}
						label={getString(properties.navigation.daybutton)}
						displayArrow={ArrowType.ARROW_LEFT}
						arrowBeforeLabel={true}
						onClick={props.onClickBack}/>
				}
			</Col>
			<Col xs={8}>
				<span className={styles.text}>{formatDateText(props.geselecteerdeDatum)}</span>
			</Col>
			<Col xs={2} className={styles.noPadding}>
				{props.vooruitKnopIsZichtbaar && <Button className={styles.right}
														 lightStyle={true}
														 label={getString(properties.navigation.daybutton)}
														 displayArrow={ArrowType.ARROW_RIGHT}
														 onClick={props.onClickForward}/>}
			</Col>
		</Row>
	)
}

export default NavigationDaysComponent
