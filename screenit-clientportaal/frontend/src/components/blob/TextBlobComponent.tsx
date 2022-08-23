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
import {useSelectedBvo} from "../../utils/Hooks"
import classNames from "classnames"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import styles from "./TextBlobComponent.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import {NavLink} from "react-bootstrap"
import SpanWithHtml from "../span/SpanWithHtml"
import ArrowIconComponent, {ArrowType} from "../vectors/ArrowIconComponent"
import BlobComponent from "./BlobComponent"

export type TextBlobComponentProps = {
	titel: string,
	tekst: string,
	extraTekst?: string,
	adresLocatie?: string,
	onLinkClick?: () => void,
	linkTekst?: string
}

const TextBlobComponent = (props: TextBlobComponentProps) => {
	const selectedBvo = useSelectedBvo()
	return (
		<BlobComponent>
			<div className={classNames(BevolkingsonderzoekStyle[selectedBvo!])}>
				<div className={styles.titelBox}>
					<span className={classNames(styles.titel, bvoStyle.bvoText)}>{props.titel}</span>
					<SpanWithHtml className={classNames(styles.tekstTitel)} value={props.tekst}/>
					{props.extraTekst && <span className={styles.extraTekst}>{props.extraTekst}</span>}
				</div>
				<div className={styles.adresBox}>
					<SpanWithHtml className={classNames(styles.adresLocatie)} value={props.adresLocatie || ""}/>
				</div>
				{(props.onLinkClick && props.linkTekst) && <div>
					<NavLink className={styles.link} onClick={props.onLinkClick}>{props.linkTekst}
						<ArrowIconComponent className={styles.arrow} type={ArrowType.ARROW_RIGHT}/>
					</NavLink>
				</div>}
			</div>
		</BlobComponent>
	)
}

export default TextBlobComponent
