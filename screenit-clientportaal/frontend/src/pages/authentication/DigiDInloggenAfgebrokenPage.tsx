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
import {Navigate, useLocation} from "react-router-dom"
import {useKeycloak} from "@react-keycloak/web"
import {useDispatch, useSelector} from "react-redux"
import {Col, Row} from "react-bootstrap"
import authenticationStyle from "./AuthenticationPage.module.scss"
import styles from "./DigiDInloggenAfgebrokenPage.module.scss"
import blob_personen from "../../scss/media/blob-personen.jpg"
import ImageBlobComponent from "../../components/blob/ImageBlobComponent"
import {State} from "../../datatypes/State"
import {setLoggingInAction} from "../../actions/AuthenticatieAction"
import {getString} from "../../utils/TekstPropertyUtil"
import classNames from "classnames"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import Button from "../../components/input/Button"
import {ArrowType} from "../../components/vectors/ArrowIconComponent"
import properties from "./DigiDInloggenAfgebrokenPage.json"
import {getBevolkingsonderzoekNederlandUrl, getBevolkingsonderzoekNederlandUrlNaam} from "../../utils/UrlUtil"
import {Location} from "history"

enum DigidLoginAfbrekenCode {
	"cancelled" = "cancelled",
	"bsn" = "bsn",
	"sofi" = "sofi",
	"error" = "error"
}

const DigiDInloggenAfgebrokenPage = () => {
	const {initialized: keycloakInitialized, keycloak} = useKeycloak()
	const dispatch = useDispatch()
	const authenticatie = useSelector((state: State) => state.authenticatie)
	const digidLoginAfbrekenCode = getDigiDLoginAfbrekenCode(useLocation())
	const toonNogmaalsInloggenButton = digidLoginAfbrekenCode === DigidLoginAfbrekenCode.cancelled || digidLoginAfbrekenCode === DigidLoginAfbrekenCode.error

	const login = useCallback(() => {
		dispatch(setLoggingInAction(true))
	}, [dispatch])

	if (keycloakInitialized && keycloak?.authenticated && authenticatie.isLoggedIn && !authenticatie.isLoggingOut) {
		return <Navigate replace to={"/"}/>
	}
	return (
		<div className={classNames(authenticationStyle.style, styles.style)}>
			<Row>
				<Col sm={8}>
					<div>
						<h1>{getString(properties["login"]["digid"][digidLoginAfbrekenCode]["title"])}</h1>
						<div className={authenticationStyle.introTextContainer}>
							<SpanWithHtml value={getString(properties["login"]["digid"][digidLoginAfbrekenCode]["tekst"])}/>
						</div>
						{toonNogmaalsInloggenButton && <div className={styles.loginDigid}>
							<Button label={getString(properties.login.button.title)} displayArrow={ArrowType.ARROW_RIGHT} onClick={login}/>
						</div>}
						<div className={styles.naarBvo}>
							<Button label={getString(properties.terug.naar, [getBevolkingsonderzoekNederlandUrlNaam()])} displayArrow={ArrowType.ARROW_LEFT}
									onClick={() => window.open(getBevolkingsonderzoekNederlandUrl(), "_self")}/>
						</div>
					</div>
				</Col>
				<Col sm={4}>
					<ImageBlobComponent image={blob_personen} className={authenticationStyle.blob}/>
				</Col>
			</Row>
		</div>
	)
}

function getDigiDLoginAfbrekenCode(location: Location) {
	let fullDigiDLoginAfbrekenCode = new URLSearchParams(location.search).get("code")
	let digidLoginAfbrekenCodeString = fullDigiDLoginAfbrekenCode !== null && fullDigiDLoginAfbrekenCode !== undefined ? fullDigiDLoginAfbrekenCode.replace("login.digid.", "") : "error"
	if (!Object.values(DigidLoginAfbrekenCode).includes(digidLoginAfbrekenCodeString as DigidLoginAfbrekenCode)) {
		digidLoginAfbrekenCodeString = "error"
	}
	return digidLoginAfbrekenCodeString as keyof typeof DigidLoginAfbrekenCode
}

export default DigiDInloggenAfgebrokenPage
