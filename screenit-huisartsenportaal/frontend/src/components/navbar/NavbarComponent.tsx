/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import classNames from "classnames"
import styles from "./NavbarComponent.module.scss"
import {Nav, Navbar, NavbarBrand, NavItem} from "react-bootstrap"
import logo from "../../styling/media/screenit_logo.png"
import {useAppSelector, useAppThunkDispatch} from "../../index"
import NavbarCollapse from "react-bootstrap/NavbarCollapse"
import NavbarToggle from "react-bootstrap/NavbarToggle"
import {Recht} from "../../state/datatypes/enums/Recht"
import {heeftRecht} from "../../state/datatypes/dto/UserDto"
import {afmelden} from "../../api/AfmeldenThunkAction"
import {loadingThunkAction} from "../../api/LoadingThunkAction"
import {useNavigate} from "react-router"
import {createActionPushToast} from "../../state/ToastsState"
import {ToastType} from "../../state/datatypes/Toast"
import React from "react"

const NavbarComponent = () => {
	const auth = useAppSelector(state => state.oauth)
	const user = useAppSelector(state => state.user)
	const navigate = useNavigate()
	const dispatch = useAppThunkDispatch()

	return <Navbar expand={"lg"} className={classNames(styles.style, "fixed-top", "navbar-dark")}>
		<NavbarBrand className={classNames(styles.navBarBrand, "ms-3")}>
			<a href={"/"}>
				<img src={logo} alt={"Logo van het huisartsportaal"}/>
			</a>
		</NavbarBrand>
		<NavbarToggle className={styles.navBarToggle} aria-controls="navigation"/>

		<NavbarCollapse id={"navigation"}>
			{user && heeftRecht(user, Recht.ROLE_AANVRAGEN) &&
				<Nav>
					{maakNavItem("/", "Labformulieren aanvragen")}
					{maakNavItem("/verrichtingen", "Overzicht verrichtingen")}
					{maakNavItem("/betalingen", "Overzicht betalingen")}
				</Nav>}

			{!!auth && <Nav className={"ms-auto"}>
				{(user && heeftRecht(user, Recht.ROLE_AANVRAGEN)) && maakNavItem("/gegevens", "Wijzig gegevens")}
				{maakNavItem("/logout", "Afmelden", () => {
					dispatch(loadingThunkAction(afmelden(auth))).then(() => {
						dispatch(createActionPushToast({type: ToastType.SUCCESS, message: "U bent uitgelogd"}))
						navigate("/")
					})
				})}
			</Nav>}
		</NavbarCollapse>
	</Navbar>
}

const maakNavItem = (path: string, naam: string, onClick?: () => void) => {
	return <NavItem className={classNames(styles.navItem, "mx-2")}>
		{!onClick ? <a href={path}>{naam}</a> : <span onClick={onClick}>{naam}</span>}
	</NavItem>
}

export default NavbarComponent
