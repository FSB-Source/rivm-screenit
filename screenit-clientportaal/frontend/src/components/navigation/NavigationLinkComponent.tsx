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
import {Link, useLocation} from "react-router-dom"
import React from "react"
import classNames from "classnames"
import styles from "./NavigationLinkComponent.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import {useSelectedBvo} from "../../utils/Hooks"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import {isExternalUrl} from "../../utils/UrlUtil"

export type NavigationLinkComponentProps = {
    url: string;
    text: string;
    bold: boolean,
    onClick?: () => void;
}

const NavigationLinkComponent = (props: NavigationLinkComponentProps) => {

    const location = useLocation()
    const selectedBvo = useSelectedBvo()

    return (
        isExternalUrl(props.url) ?
            <a href={props.url}
               aria-controls={"navbar-collapse"}
               className={classNames(selectedBvo && BevolkingsonderzoekStyle[selectedBvo], bvoStyle.bvoNav, styles.navItem, props.bold && styles.navItemBvo, "nav-link", location.pathname.startsWith(props.url) && styles.navItemActive)}
               rel="noopener noreferrer">
                {props.text}
            </a> :
            <Link to={props.url}
                  aria-controls={"navbar-collapse"}
                  onClick={props.onClick}
                  className={classNames(selectedBvo && BevolkingsonderzoekStyle[selectedBvo], bvoStyle.bvoNav, styles.navItem, props.bold && styles.navItemBvo, "nav-link", location.pathname.startsWith(props.url) && styles.navItemActive)}>
                {props.text}
            </Link>

    )

}

export default NavigationLinkComponent
