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
import styles from "./BezwaarPage.module.scss"
import arrowDown from "../../scss/media/arrow-down.svg"
import arrowUp from "../../scss/media/arrow-up.svg"

export type BezwaarInformatieBlokProps = {
	abstract: string;
	children?: React.ReactNode
}

const BezwaarInformatieBlok = (props: BezwaarInformatieBlokProps) => {
	const [toonMeerInformatie, setToonMeerInformatie] = useState<boolean>(false)

	function toggleMeerInformatie() {
        setToonMeerInformatie(!toonMeerInformatie)
    }

    return (
        <div className={styles.informatie}>
            {props.abstract && <div className={styles.abstract} dangerouslySetInnerHTML={{__html: props.abstract}}/>}
            {props.children &&
            <>
                {!toonMeerInformatie &&
                <div className={styles.toonVerbergInformatie} onClick={toggleMeerInformatie}>Toon informatie<img alt="" className={styles.arrow} src={arrowDown}/></div>}
                {toonMeerInformatie && <>
                    <div className={styles.toonVerbergInformatie} onClick={toggleMeerInformatie}>Verberg informatie<img alt="" className={styles.arrow} src={arrowUp}/></div>
                    <div>
                        {props.children}
                    </div>
                </>}
            </>}
        </div>
    )
}

export default BezwaarInformatieBlok
