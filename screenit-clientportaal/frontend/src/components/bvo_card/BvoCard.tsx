/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import React from "react"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam, BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import styles from "./BvoCard.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import classNames from "classnames"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import {BvoParameters} from "../../datatypes/landing/BvoParameters"
import {Client} from "../../datatypes/Client"
import {getString} from "../../utils/TekstPropertyUtil"

import properties from "./BvoCard.json"
import VerticalDividerComponent from "../vectors/VerticalDividerComponent"
import SpanWithHtml from "../span/SpanWithHtml"
import {useWindowDimensions} from "../../utils/Hooks"

export type BvoCardProps = {
	bvo: Bevolkingsonderzoek;
	clickable: boolean;
	onClick: () => void;
}

const BvoCard = (props: BvoCardProps) => {
	const {width} = useWindowDimensions()
	const bvoNaam = props.bvo === (width > 576 && Bevolkingsonderzoek.CERVIX) ? properties.BMHK_NAAM_GESPLITST : BevolkingsonderzoekNaam[props.bvo]
	const client = useSelector((state: State) => state.client)
	const bvoParameters: BvoParameters | undefined = useSelector((state: State) => {
		switch (props.bvo) {
			case Bevolkingsonderzoek.MAMMA:
				return state.landingOverzicht.mammaParameters
			case Bevolkingsonderzoek.CERVIX:
				return state.landingOverzicht.cervixParameters
			case Bevolkingsonderzoek.COLON:
				return state.landingOverzicht.colonParameters
			default:
				return undefined
		}
	})

	return (
		<div className={classNames(styles.bvoCard, BevolkingsonderzoekStyle[props.bvo], !props.clickable && styles.disabled)} onClick={() => props.clickable && props.onClick()}>
			<VerticalDividerComponent className={styles.verticalRectangle}/>

			<span className={bvoStyle.bvoText}>Bevolkingsonderzoek</span>
			<h1 className={styles.bvoNaam}>{bvoNaam}</h1>

			<div className={styles.bvoEventContainer}>
				{bvoParameters ? getBvoStatus(client, bvoParameters, props.bvo) : <span className={styles.bvoEventLabel}>Laden...</span>}
			</div>
		</div>
	)

}

const getBvoStatus = (client: Client, bvoParameters: BvoParameters, bvo: Bevolkingsonderzoek): JSX.Element => {
	return <SpanWithHtml className={styles.bvoDoelgroepUitleg}
						 value={getString(getManVrouwTekst(bvo), [bvoParameters.leeftijdOndergrens, bvoParameters.leeftijdBovengrens])}/>
}

function getManVrouwTekst(bvo: Bevolkingsonderzoek): string {
	switch (bvo) {
		case Bevolkingsonderzoek.COLON:
			return properties.MANNEN_VROUWEN_TUSSEN
		default:
			return properties.VROUWEN_TUSSEN
	}
}

export default BvoCard
