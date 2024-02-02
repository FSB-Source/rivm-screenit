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
import {Container} from "react-bootstrap"
import styles from "./BasePage.module.scss"
import bvoStyle from "../components/BvoStyle.module.scss"
import {useSelectedBvo} from "../utils/Hooks"
import classNames from "classnames"
import {BevolkingsonderzoekStyle} from "../datatypes/Bevolkingsonderzoek"
import KruimelpadComponent from "../components/kruimelpad/KruimelpadComponent"
import SpanWithHtml from "../components/span/SpanWithHtml"
import HintComponent from "../components/hint/HintComponent"
import properties from "./ActieBasePage.json"
import {getString} from "../utils/TekstPropertyUtil"

export type ActieBasePageProps = {
    className?: string,
    bvoName: string,
    title: string,
    description?: string,
    hintBegin?: string,
    hintBeginClassName?: string,
    children: React.ReactNode,
    hintEinde?: string
    hintEindeClassName?: string,
}

const ActieBasePage = (props: ActieBasePageProps) => {

    const selectedBvo = useSelectedBvo()

    return (
        <Container fluid
				   className={classNames(styles.content, styles.slim, selectedBvo && BevolkingsonderzoekStyle[selectedBvo], props.className)}>
			<KruimelpadComponent/>
			<h4 className={bvoStyle.bvoText}>{getString(properties.title, [props.bvoName])}</h4>

			<h1>{props.title}</h1>

			{props.description && <SpanWithHtml value={props.description}/>}

			{props.hintBegin && <HintComponent className={props.hintBeginClassName}><SpanWithHtml value={props.hintBegin}/></HintComponent>}

			<div className={styles.childrenContainer}>
                {props.children}
            </div>

            {props.hintEinde && <HintComponent className={props.hintEindeClassName}><SpanWithHtml value={props.hintEinde}/></HintComponent>}

        </Container>
    )

}

export default ActieBasePage
