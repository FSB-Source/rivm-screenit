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
import styles from "./BezwaarOverigeMeerInformatieBlokken.module.scss"
import {getString} from "../../utils/TekstPropertyUtil"
import {BezwaarOverigeType} from "../../datatypes/BezwaarOverigeType"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import properties from "./BezwaarOverigeType.json"

export type BezwaarOverigeMeerInformatieBlokProps = {
    types: BezwaarOverigeType[]
}
const BezwaarOverigeMeerInformatieBlokken = (props: BezwaarOverigeMeerInformatieBlokProps) => {

    return (
        <>
            {props.types.map(ob => (
                <div key={ob} className={styles.overigeMeerInformatieBlok}>
                    <SpanWithHtml value={getString(properties[ob]["title"])}/>
                    <SpanWithHtml value={getString(properties[ob]["body"])}/>
                </div>
            ))
            }
        </>
    )
}

export default BezwaarOverigeMeerInformatieBlokken
