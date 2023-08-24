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
import styles from "./BeforeSearching.module.scss"
import SpanWithHtml from "../span/SpanWithHtml"

export type BeforeSearchingProps = {
	text: string
}

const BeforeSearching = (props: BeforeSearchingProps) => {

	return (
		<div className={styles.beforeSearching}>
			<svg xmlns="http:
                <g id="Element_Arrow_handdrawn" data-name="Element/Arrow handdrawn" transform="translate(1.228 1.239)">
                    <g id="Group_4" data-name="Group 4" transform="matrix(0.966, 0.259, -0.259, 0.966, 3.868, 0.215)">
                        <path id="Path" d="M62,.628c-38-3.5-62,12.5-62,12.5" transform="translate(0 0)" fill="none" stroke="#c2c4c7" strokeLinecap="round" strokeLinejoin="round"
                              strokeMiterlimit="10" strokeWidth="2"/>
                        <path id="Path_2" data-name="Path 2" d="M7.5,0,0,13H13" transform="translate(0 0.128)" fill="none" stroke="#c2c4c7" strokeLinecap="round"
                              strokeLinejoin="round" strokeMiterlimit="10" strokeWidth="2"/>
                    </g>
                </g>
            </svg>
            <SpanWithHtml value={props.text}/>
        </div>
    )

}

export default BeforeSearching;
