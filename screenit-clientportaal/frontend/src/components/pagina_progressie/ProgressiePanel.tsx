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
import styles from "./PaginaProgressie.module.scss"
import React from "react"
import classNames from "classnames"
import ArrowIconComponent, {ArrowType} from "../vectors/ArrowIconComponent"

export type ProgressiePanelProps = {
	aantalPaginas: number,
	huidigePagina: number,
	onTerug?: () => void
}

const ProgressiePanel = (props: ProgressiePanelProps) => {
	return (<div className={styles.onderkant}>

			{props.onTerug !== undefined && <div className={styles.terugknop} onClick={props.onTerug}>
				<ArrowIconComponent type={ArrowType.ARROW_LEFT}/> Ga terug
			</div>}
			{props.onTerug === undefined && <div className={styles.filler}/>}
			<div className={styles.progressieBollen}>
				{[...Array(props.aantalPaginas)].map((_e, index) => <div key={"bol_" + (index + 1)} className={getStyleClass(props, index)}></div>)}
			</div>
		</div>
	)
}

function getStyleClass(props: ProgressiePanelProps, bolIndex: number): string {
	if ((bolIndex + 1) <= props.huidigePagina) {
		return classNames(styles.bol, styles.actief)
	} else {
		return styles.bol
	}
}

export default ProgressiePanel
