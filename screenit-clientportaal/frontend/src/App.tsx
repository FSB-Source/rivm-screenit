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
import "bootstrap/dist/css/bootstrap.min.css"
import "./scss/style.scss"
import styles from "./App.module.scss"
import {Container} from "react-bootstrap"
import {AppRoutes} from "./routes"
import HeaderComponent from "./components/header/HeaderComponent"
import FooterComponent from "./components/footer/FooterComponent"
import ActiveToastsComponent from "./components/toast/ActiveToastsComponent"
import {useGotoPageTop, useHideAllToasts, useRefreshBvoDossier, useRefreshClient, useRefreshEnvironmentInfo} from "./utils/Hooks"

const App = () => {
	useRefreshEnvironmentInfo()
	useRefreshClient()
	useRefreshBvoDossier()
	useHideAllToasts()
	useGotoPageTop()

	return (
		<div className={styles.app}>
			<Container className={styles.appBody} fluid>
				<ActiveToastsComponent/>
				<HeaderComponent/>
				<Container fluid className={styles.appContent}>
					<AppRoutes/>
				</Container>
			</Container>
			<FooterComponent/>
		</div>
	)
}

export default App
