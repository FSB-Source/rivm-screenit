/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import styles from "./App.module.scss"
import ToastOverlay from "./components/overlay/toast/ToastOverlay"
import LoadingSpinnerOverlay from "./components/overlay/loading/LoadingSpinnerOverlay"
import NavbarComponent from "./components/navbar/NavbarComponent"
import BuildInfoComponent from "./components/buildinfo/BuildInfoComponent"
import {Container} from "react-bootstrap"
import classNames from "classnames"
import properties from "./App.json"
import {getString} from "./util/TekstPropertyUtil"

const App = ({children}: { children: JSX.Element }) => {
	return (
		<>
			<ToastOverlay/>
			<LoadingSpinnerOverlay/>
			<NavbarComponent/>
			<Container fluid className={styles.content}>
				{children}
			</Container>
			<BuildInfoComponent/>
			<a href={getString(properties.responsibleDisclosure.url)} rel="noopener noreferrer" target="_blank"
			   className={classNames(styles.responsibleDisclosure)}>{getString(properties.responsibleDisclosure.label)}</a>
		</>
	)
}

export default App
