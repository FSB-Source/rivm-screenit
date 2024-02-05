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
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import classNames from "classnames"
import bvoStyles from "../BvoStyle.module.scss"
import styles from "./InleidingComponent.module.scss"
import React from "react"
import {useSelectedBvo} from "../../utils/Hooks"
import BvoUrlComponent from "../bvo_url/BvoUrlComponent"
import SpanWithHtml from "../span/SpanWithHtml"
import HintComponent from "../hint/HintComponent"
import properties from "./InleidingComponent.json"
import {getString} from "../../utils/TekstPropertyUtil"

export type InleidingComponentProps = {
	bvoNaam: string,
	groteTitel: string,
	inleidingBvoTekst: string,
	hintTekst?: string,
	link?: string,
	linkTekst?: string,
	toonAlgemeneInleidingTekst: boolean,
	volgendeUitnodigingTekst?: string
}

const InleidingComponent = (props: InleidingComponentProps) => {

	const selectedBvo = useSelectedBvo()

	return (
		<div className={BevolkingsonderzoekStyle[selectedBvo!]}>
			<h4
				className={classNames(bvoStyles.bvoText)}>{getString(properties.bvo_title, [props.bvoNaam])}</h4>
			<h1 className={styles.bvoNaam}>{props.groteTitel}</h1>
			<div className={styles.infoContainer}>
				<SpanWithHtml className={styles.infoText} value={props.inleidingBvoTekst}/>
			</div>
			{props.hintTekst && <HintComponent><SpanWithHtml value={props.hintTekst}/></HintComponent>}
			{props.volgendeUitnodigingTekst && <SpanWithHtml value={props.volgendeUitnodigingTekst}/>}
			{props.link && props.linkTekst && <BvoUrlComponent link={props.link} tekst={props.linkTekst}/>}
			{props.toonAlgemeneInleidingTekst && <SpanWithHtml className={styles.infoText} value={getString(properties.inleiding_algemeen)}/>}
		</div>
	)
}

export default InleidingComponent
