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
import * as React from "react"
import {useCallback} from "react"
import {useKeycloak} from "@react-keycloak/web"
import {useDispatch, useSelector} from "react-redux"
import {Col, Row} from "react-bootstrap"
import authenticationStyle from "./AuthenticationPage.module.scss"
import styles from "./LoginPage.module.scss"
import blob_personen from "../../scss/media/blob-personen.jpg"
import ImageBlobComponent from "../../components/blob/ImageBlobComponent"
import {State} from "../../datatypes/State"
import {setLoggingInAction} from "../../actions/AuthenticatieAction"
import {getString} from "../../utils/TekstPropertyUtil"
import classNames from "classnames"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import Button from "../../components/input/Button"
import {ArrowType} from "../../components/vectors/ArrowIconComponent"
import properties from "./LoginPage.json"
import {Navigate} from "react-router-dom"

const LoginPage = () => {
	const {initialized: keycloakInitialized, keycloak} = useKeycloak()
	const dispatch = useDispatch()
	const authenticatie = useSelector((state: State) => state.authenticatie)

	const login = useCallback(() => {
		dispatch(setLoggingInAction(true))
	}, [dispatch])

	if (keycloakInitialized && keycloak?.authenticated &&
		((authenticatie.isLoggedIn && !authenticatie.isLoggingOut) ||
			(!authenticatie.isLoggingIn && !authenticatie.isLoggingOut && !authenticatie.isLoggedIn && !authenticatie.isSessionExpired))) {
		return <Navigate replace to={"/"}/>
	} else {
		return (
			<div className={classNames(authenticationStyle.style, styles.style)}>
				<Row>
					<Col sm={8}>
						{(keycloak?.authenticated && authenticatie.isLoggingIn && !authenticatie.isLoggingOut) ? <div>
							<h1>{getString(properties.title.logging_in)}</h1>
						</div> : <div>
							<h1>{authenticatie.isSessionExpired ? getString(properties.title.opnieuw_inloggen) : getString(properties.title.inloggen)}</h1>
							<div className={authenticationStyle.introTextContainer}>
								<SpanWithHtml
									value={authenticatie.isSessionExpired ? getString(properties.description.opnieuw_inloggen) : getString(properties.description.inloggen)}/>
							</div>
							<div className={styles.loginDigid}>
								<Button label={getString(properties.button)} displayArrow={ArrowType.ARROW_RIGHT} onClick={login}/>

							</div>
						</div>}
					</Col>
					<Col sm={4}>
						<ImageBlobComponent image={blob_personen} className={authenticationStyle.blob}/>
					</Col>
				</Row>
			</div>
		)
	}
}

export default LoginPage
