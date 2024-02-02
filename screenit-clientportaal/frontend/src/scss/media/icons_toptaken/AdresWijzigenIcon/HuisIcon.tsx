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
import React from "react"
import styles from "./HuisIcon.module.scss"

export type HuisIconProps = {
    className?: string
}

const HuisIcon = (props: HuisIconProps) => {
    return (
        <svg className={props.className} version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 47 47" xmlSpace="preserve">
            <g>
                <polyline className={styles.std0}
                          points="7.6,25.1 7.6,44.2 19.1,44.2 19.1,30.8 28.6,30.8 28.6,44.2 40.1,44.2 40.1,26 	"/>
                <polyline className={styles.std0} points="1.9,24.1 23.8,2.2 45.8,24.1 	"/>
                <polyline className={styles.std0} points="31.5,5.1 38.2,5.1 38.2,11.8 	"/>
            </g>
        </svg>
    )
}

export default HuisIcon
