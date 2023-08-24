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
import styles from "./UistellenIcon.module.scss"

const UitstellenIcon = () => {
    return (
        <svg version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 47 47" xmlSpace="preserve">
            <g>
                <g>
                    <circle className={styles.st0} cx="36.5" cy="36.5" r="10"/>
                    <polyline className={styles.st1} points="40.5,38.5 36.5,38.5 36.5,31.5 		"/>
                </g>
                <g>
                    <line className={styles.st1} x1="24.5" y1="28.5" x2="24.5" y2="22.9"/>
                    <path className={styles.st1} d="M14.5,22.9v5.6l-10,3.6c-2.4,0.9-4,3.1-4,5.6v4.8h22"/>
                    <ellipse className={styles.st0} cx="19.5" cy="12.5" rx="10" ry="12"/>
                    <path className={styles.st0}
                          d="M29.4,11.4c-0.3,0-0.6,0-0.9,0.1c-3.4,0.7-5.6-0.6-7.5-3.9c-1.1,2.2-4.6,3.9-7.5,3.9c-1.4,0-2.6-0.3-3.9-0.9"
                    />
                </g>
            </g>
        </svg>
    )
}

export default UitstellenIcon
