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
import BasePopup from "../../../../components/popup/BasePopup"
import {Col, NavLink, Row} from "react-bootstrap"
import classNames from "classnames"
import bvoStyles from "../../../../components/BvoStyle.module.scss"
import styles from "./HuisartsBevestigingsPopup.module.scss"
import {useSelectedBvo} from "../../../../utils/Hooks"
import {BevolkingsonderzoekStyle} from "../../../../datatypes/Bevolkingsonderzoek"
import {useThunkDispatch} from "../../../../index"
import {getString} from "../../../../utils/TekstPropertyUtil"
import VerticalDividerComponent from "../../../../components/vectors/VerticalDividerComponent"
import {Huisarts} from "../../../../datatypes/Huisarts"
import {koppelHuisarts, ontkoppelHuisarts} from "../../../../api/HuisartsThunkAction"
import {concatWithSpace} from "../../../../utils/StringUtil"
import properties from "./HuisartsBevestigingsPopup.json"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import SubmitButton from "../../../../components/input/SubmitButton"

export type HuisartsBevestigingsPopupProps = {
    huisarts?: Huisarts,
    geenHuisartsOpie?: string
    type: HuisartsBevestigingsPopupType,
    onSuccess?: () => void,
    onClose: () => void
}

export enum HuisartsBevestigingsPopupType {
    "BEVESTIGEN",
    "VERWIJDEREN"
}

export const HuisartsBevestigingsPopup = (props: HuisartsBevestigingsPopupProps) => {
    const bvo = useSelectedBvo()
    const dispatch = useThunkDispatch()

    const bevestigHuisarts = () => {
        switch (props.type) {
            case HuisartsBevestigingsPopupType.VERWIJDEREN:
                dispatch(ontkoppelHuisarts(bvo)).then(
                    () => {
                        props.onSuccess && props.onSuccess()
                    },
                    () => {
                        props.onClose()
                    },
                )
                break
            case HuisartsBevestigingsPopupType.BEVESTIGEN:
                if (props.huisarts) {
                    dispatch(koppelHuisarts(props.huisarts, bvo)).then(
                        () => {
                            props.onSuccess && props.onSuccess()
                        },
                        () => {
                            props.onClose()
                        },
                    )
                }
                break
        }
    }

    return (

        <BasePopup title={props.type === HuisartsBevestigingsPopupType.BEVESTIGEN ? getString(properties.bevestigen.title) : getString(properties.verwijderen.title)}
                   description={props.type === HuisartsBevestigingsPopupType.BEVESTIGEN ? getString(properties.bevestigen.description) : getString(properties.verwijderen.description)}
                   children={
                       <div>
						   <div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.huisartsDiv)}>
							   <VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={1}/>
							   {props.huisarts ? <Row className={styles.huisartsGegevens}>
									   <Col sm={6}>
										   <span className={classNames(bvoStyles.bvoText)}>Naam</span>
										   <span>{props.huisarts.praktijknaam}</span>
										   <span>{concatWithSpace(props.huisarts.voorletters, props.huisarts.achternaam)}</span>
									   </Col>
									   <Col sm={6} className={styles.locatieColumn}>
										   <span className={classNames(bvoStyles.bvoText)}>Locatie</span>
										   <span>{concatWithSpace(props.huisarts.adres.straat, props.huisarts.adres.huisnummer, props.huisarts.adres.huisnummerToevoeging, props.huisarts.adres.huisletter)}</span>
										   <span>{concatWithSpace(props.huisarts.adres.postcode, props.huisarts.adres.plaats)}</span>
									   </Col>
								   </Row> :
								   <div className={styles.huisartsRow}>
									   <span>{props.geenHuisartsOpie}</span>
								   </div>
							   }
						   </div>
						   <div className={styles.buttons}>
							   <SubmitButton displayArrow={ArrowType.ARROW_RIGHT}
											 label={props.type === HuisartsBevestigingsPopupType.BEVESTIGEN ? getString(properties.bevestigen.button_bevestigen) : getString(properties.verwijderen.button_bevestigen)}
											 onClick={() => {
												 bevestigHuisarts()
											 }}/>
							   <NavLink onClick={props.onClose} className={styles.andereHuisarts}>
								   {props.type === HuisartsBevestigingsPopupType.BEVESTIGEN ? getString(properties.bevestigen.button_annuleren) : getString(properties.verwijderen.button_annuleren)}
							   </NavLink>
						   </div>
					   </div>
                   }/>

    )

}
