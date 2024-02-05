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
import authenticationStyle from "./AuthenticationPage.module.scss"
import React, {useCallback} from "react"
import {Col, Row} from "react-bootstrap"
import {getString} from "../../utils/TekstPropertyUtil"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import ImageBlobComponent from "../../components/blob/ImageBlobComponent"
import blob_personen from "../../scss/media/blob-personen.jpg"
import {getBevolkingsonderzoekNederlandUrl, getBevolkingsonderzoekNederlandUrlNaam} from "../../utils/UrlUtil"
import properties from "./NietInBevolkingsonderzoekPage.json"
import Button from "../../components/input/Button"
import {ArrowType} from "../../components/vectors/ArrowIconComponent"
import {useNavigate} from "react-router-dom"

const NietInBevolkingsonderzoekPage = () => {
	const navigate = useNavigate()

	const logoutCallback = useCallback(() => {
		navigate("/logout")
	}, [navigate])

	return <div className={authenticationStyle.style}>
		<Row>
			<Col sm={8}>
				<div className={authenticationStyle.introTextContainer}>
					<SpanWithHtml value={getString(properties.description, [getBevolkingsonderzoekNederlandUrl(), getBevolkingsonderzoekNederlandUrlNaam()])}/>
				</div>
				<div>
					<Button label={properties.uitloggen} onClick={logoutCallback} arrowBeforeLabel={true} displayArrow={ArrowType.ARROW_LEFT}/>
				</div>
			</Col>
			<Col sm={4}>
				<ImageBlobComponent image={blob_personen} className={authenticationStyle.blob}/>
			</Col>
		</Row>
	</div>

}

export default NietInBevolkingsonderzoekPage
