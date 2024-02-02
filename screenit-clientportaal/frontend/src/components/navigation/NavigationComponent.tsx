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
import React, {useEffect, useState} from "react"
import {Nav, Navbar, NavItem} from "react-bootstrap"
import styles from "./NavigationComponent.module.scss"
import classNames from "classnames"
import {Link, useNavigate} from "react-router-dom"
import NavigationLinkComponent from "./NavigationLinkComponent"
import {useRegio} from "../../utils/Hooks"
import {getLogoutLink, getPersoonNaam} from "../header/HeaderComponent"
import {useSelector} from "react-redux"
import MijnBevolkingsOnderzoekLogo from "../../scss/media/MijnBevolkingsOnderzoekLogo"
import {State} from "../../datatypes/State"
import {getContactUrl} from "../../utils/UrlUtil"

const NavigationComponent = () => {
	const [sticky, setSticky] = useState(true)
	const [expanded, setExpanded] = useState(false)

	const persoon = useSelector((state: State) => state.client.persoon)
	const landingOverzicht = useSelector((state: State) => state.landingOverzicht)
	const nietTonenHamburger = persoon.id !== undefined

	const navigate = useNavigate()
	const regio = useRegio()

	useEffect(() => {
		fixNavBarPositieBijScrollen()
	}, [])

	const hideNavbar = () => setTimeout(() => setExpanded(false), 200)

	return (
		<Navbar expand={"lg"} expanded={expanded}
				className={classNames(styles.navBar, "justify-content-between", sticky ? "sticky-top" : "fixed-top", !sticky && styles.navBarFixed)}>
			<Navbar.Brand>
				<Link to={"/"} onClick={hideNavbar}>
					<MijnBevolkingsOnderzoekLogo/>
				</Link>
			</Navbar.Brand>
			{nietTonenHamburger && <Navbar.Toggle className={styles.hamburger} aria-controls={"navbar-collapse"}
												  onClick={() => setExpanded(!expanded)}/>}
			{nietTonenHamburger && <Navbar.Collapse id={"navbar-collapse"} className={styles.nav}>
				<Nav className={"ml-auto"}>
					{landingOverzicht.behoortTotMammaDoelgroep &&
						<NavItem>
							<NavigationLinkComponent url={"/mamma"}
													 text={"Borstkanker"}
													 bold={true}
													 onClick={hideNavbar}/>
						</NavItem>
					}
					{landingOverzicht.behoortTotCervixDoelgroep &&
						<NavItem>
							<NavigationLinkComponent url={"/cervix"}
													 text={"Baarmoederhalskanker"}
													 bold={true}
													 onClick={hideNavbar}/>
						</NavItem>
					}
					{landingOverzicht.behoortTotColonDoelgroep &&
						<NavItem>
							<NavigationLinkComponent url={"/colon"}
													 text={"Darmkanker"}
													 bold={true}
													 onClick={hideNavbar}/>
						</NavItem>
					}
					<NavItem>
						<NavigationLinkComponent url={"/profiel"}
												 text={"Mijn profiel"}
												 bold={false}
												 onClick={hideNavbar}/>
					</NavItem>
					<NavItem>
						<NavigationLinkComponent url={getContactUrl(regio)}
												 text={"Contact"}
												 bold={false}/>
					</NavItem>

					{<div className={styles.clientLogout}>
						<NavItem>
							<span>{getPersoonNaam(persoon)}</span>
						</NavItem>
						<NavItem>
							{getLogoutLink(() => navigate("/logout"))}
						</NavItem>
					</div>}
				</Nav>
			</Navbar.Collapse>}
		</Navbar>
	)

	function fixNavBarPositieBijScrollen() {
		window.addEventListener("scroll", () => {
			setSticky(document.body.scrollTop < 100 && document.documentElement.scrollTop < 100)
		})
	}
}

export default NavigationComponent
