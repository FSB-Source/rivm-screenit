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
import React, {useState} from "react"
import styles from "./BezwaarInformatieBlok.module.scss"
import ArrowIconComponent, {ArrowType} from "../../components/vectors/ArrowIconComponent"
import classNames from "classnames"
import SpanWithHtml from "../../components/span/SpanWithHtml"

export type BezwaarInformatieBlokProps = {
	abstract: string;
	meer?: string;
	standalone?: boolean;
}

const BezwaarInformatieBlok = (props: BezwaarInformatieBlokProps) => {
	const [toonMeerInformatie, setToonMeerInformatie] = useState<boolean>(false)
	function toggleMeerInformatie() {
        setToonMeerInformatie(!toonMeerInformatie)
    }

    return (
		<div className={classNames(styles.informatie, props.standalone ? styles.standalone : "")}>
            {props.abstract && <div className={styles.abstract} dangerouslySetInnerHTML={{__html: props.abstract}}/>}
			{props.meer &&
            <>
                {!toonMeerInformatie &&
					<div className={styles.toonVerbergInformatie} onClick={toggleMeerInformatie}>Toon informatie<ArrowIconComponent type={ArrowType.ARROW_DOWN}
																																	className={styles.arrow}/></div>}
                {toonMeerInformatie && <>
					<div className={styles.toonVerbergInformatie} onClick={toggleMeerInformatie}>Verberg informatie<ArrowIconComponent type={ArrowType.ARROW_UP}
																																	   className={styles.arrow}/></div>
					<div>
						{props.meer && <SpanWithHtml className={styles.meerTekst} value={props.meer}/>}
					</div>
                </>}
            </>}
        </div>
    )
}

export default BezwaarInformatieBlok
