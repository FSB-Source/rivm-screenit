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
import React, {useContext} from "react"
import {Col, Row} from "react-bootstrap"
import classNames from "classnames"
import styles from "./HeaderComponent.module.scss"
import NavigationComponent from "../navigation/NavigationComponent"
import {useSelector} from "react-redux"
import {getBevolkingsonderzoekNederlandUrl, getBevolkingsonderzoekNederlandUrlNaam} from "../../utils/UrlUtil"
import {State} from "../../datatypes/State"
import {Persoon} from "../../datatypes/Persoon"
import {useNavigate} from "react-router-dom"
import {KeycloakContext} from "../KeycloakProvider"

const HeaderComponent = () => {
	const {keycloak} = useContext(KeycloakContext)
	const persoon = useSelector((state: State) => state.client.persoon)
	const navigate = useNavigate()

	return (
		<section id={"page-header"} className={styles.header}>
			<Row className={classNames(styles.headerTopLinks, "align-items-center")}>
				<Col md={6} className={"text-left"}>
					<a href={getBevolkingsonderzoekNederlandUrl()}>
						<i className="material-icons">arrow_back</i><span>Terug naar {getBevolkingsonderzoekNederlandUrlNaam()}</span>
					</a>
				</Col>
				<Col md={6} className={"text-right"}>
					{persoon.id !== undefined && (
						<><span>{getPersoonNaam(persoon)}</span>
							{!!keycloak.authenticated && getLogoutLink(() => navigate("/logout"))}
						</>
					)}
				</Col>
			</Row>

			<div className={styles.navigationContainer}>
				<NavigationComponent/>
			</div>
		</section>
	)

}

export const getPersoonNaam = (persoon: Persoon) => {
	return `${persoon.voorletters} ${persoon.aanspreekTussenvoegselEnAchternaam}`
}

export const getLogoutLink = (onClick: () => void) => {
	return <span className={styles.logout} onClick={onClick}>
        Uitloggen
    </span>
}

export default HeaderComponent
