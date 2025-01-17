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
import React from "react"
import {Huisarts} from "../../../../datatypes/Huisarts"
import classNames from "classnames"
import hintComponentStyles from "../../../../components/hint/HintComponent.module.scss"
import {concatWithSpace} from "../../../../utils/StringUtil"
import properties from "./HuisartsHintComponent.json"

export type HuisartsHintComponentProps = {
    className?: string
    isBold?: boolean
    huisarts?: Huisarts
    geenHuisartsOptie?: string
}

const HuisartsHintComponent = (props: HuisartsHintComponentProps) => {

    return (
        <div className={classNames(props.className, hintComponentStyles.hintContainer)} style={{fontWeight: props.isBold ? "bold" : "normal"}}>
            {props.huisarts ?
                <div><span>{concatWithSpace(props.huisarts.voorletters, props.huisarts.achternaam)}</span>
                    <br/>
                    <span>{props.huisarts.praktijknaam}</span>
                    <br/>
                    <span>{concatWithSpace(props.huisarts.adres.straat, props.huisarts.adres.huisnummer)}</span>
                    <br/>
                    <span>{concatWithSpace(props.huisarts.adres.postcode, props.huisarts.adres.plaats)}</span>
                </div>
                : props.geenHuisartsOptie ?
                    <div>
                        <span>{props.geenHuisartsOptie}</span>
                    </div>
                    : properties.empty}
        </div>
    )
}

export default HuisartsHintComponent
