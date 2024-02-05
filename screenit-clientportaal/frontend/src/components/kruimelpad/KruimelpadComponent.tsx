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
import styles from "./KruimelpadComponent.module.scss"
import bvoStyles from "../BvoStyle.module.scss"
import routes from "../../routes/routes"
import {Link, matchPath, useLocation, useNavigate} from "react-router-dom"
import classNames from "classnames"
import Button from "../input/Button"
import ArrowIconComponent, {ArrowType} from "../vectors/ArrowIconComponent"
import HuisIcon from "../../scss/media/icons_toptaken/AdresWijzigenIcon/HuisIcon"
import {useSelectedBvo} from "../../utils/Hooks"
import {BevolkingsonderzoekToptaakStyle} from "../../datatypes/Bevolkingsonderzoek"

export type KruimelpadComponentProps = {
	className?: string
}

const KruimelpadComponent = (props: KruimelpadComponentProps) => {
	const location = useLocation()
	const currentPath = location.pathname
	const navigate = useNavigate()
	const bvo = useSelectedBvo()!

	const crumbs = routes
		.filter(route => matchPath(route.path + "*", currentPath))
		.map(({path, ...routeProps}) => {
			const matchedPath = matchPath(path, currentPath)
			let currentRegexIndex = 0
			return !(matchedPath?.params) ? {path: path, ...routeProps} : {
				...routeProps,
				path: String(path).split("/").map((subPath) => {
					const containsRegexCharacters = subPath.match(/(\(|\)|[|]|\*)/g)
					return containsRegexCharacters ? Object.values(matchedPath.params)[currentRegexIndex++] : subPath
				}).join("/"),
			};
		})
	return (
		<div className={classNames(styles.style, props.className)}>
			<Button label={"Terug"} lightStyle={true} displayArrow={ArrowType.ARROW_LEFT} arrowBeforeLabel={true} onClick={() => {
				navigate(-1)
			}} className={bvoStyles.light}/>
			{crumbs.filter(crumb => crumb.path && crumb.name).map((crumb, index) => {
				if (index === 0) {
					return <Link key={index} to={String(crumb.path)}>
						<HuisIcon className={classNames(styles.homeIcon, BevolkingsonderzoekToptaakStyle[bvo])}/>
						<ArrowIconComponent type={ArrowType.CHEVRON_RIGHT}/>
					</Link>
				}
				return <Link key={index} to={String(crumb.path)} className={classNames(styles.style)}>
					<ArrowIconComponent type={ArrowType.CHEVRON_RIGHT}/><span>{crumb.name}</span>
				</Link>
			})}
		</div>
	)

}

export default KruimelpadComponent
