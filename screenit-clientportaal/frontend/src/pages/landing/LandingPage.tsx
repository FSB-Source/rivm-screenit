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
import React, {useEffect} from "react"
import {Col, Container, Row} from "react-bootstrap"
import styles from "./LandingPage.module.scss"
import {useDispatch, useSelector} from "react-redux"
import BvoSelectieComponent from "../../components/bvo_selectie/BvoSelectieComponent"
import BvoUrlComponent from "../../components/bvo_url/BvoUrlComponent"
import {Geslacht} from "../../datatypes/Geslacht"
import blob_personen from "../../scss/media/blob-personen.jpg"
import ImageBlobComponent from "../../components/blob/ImageBlobComponent"
import {nu} from "../../utils/DateUtil"
import {getBevolkingsonderzoekNederlandUrl, getBevolkingsonderzoekNederlandUrlNaam} from "../../utils/UrlUtil"
import {getLandingOverzicht} from "../../api/LandingpageThunkAction"
import properties from "./LandingPage.json"
import {getString} from "../../utils/TekstPropertyUtil"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import {Persoon} from "../../datatypes/Persoon"
import {State} from "../../datatypes/State"

const LandingPage = () => {
	const dispatch = useDispatch()
	const persoon = useSelector((state: State) => state.client.persoon)

	useEffect(() => {
		dispatch(getLandingOverzicht())
	}, [dispatch])

	return (
		<Container fluid className={styles.content}>
			<Row>
				<Col lg={8}>
					<span className={styles.greetingText}>{getGreeting()},</span>
					<h1 className={styles.personName}>{getPersoonAanhef(persoon)}</h1>
					<div className={styles.infoContainer}>
						<SpanWithHtml className={styles.infoText} value={getString(properties.inleiding)}/>
					</div>
					<BvoUrlComponent link={getBevolkingsonderzoekNederlandUrl()}
									 tekst={getString(properties.link, [getBevolkingsonderzoekNederlandUrlNaam()])}/>
				</Col>
				<Col lg={4}>
					<ImageBlobComponent image={blob_personen} className={styles.blob}/>
				</Col>
			</Row>

			<h5 className={styles.sectieHeader}>{getString(properties.bvocard)}</h5>
			<BvoSelectieComponent/>
		</Container>
	)

}

const getGreeting = (): string => {
	const currentHour = nu().getHours()
	if (currentHour < 12) {
		return getString(properties.begroeting.ochtend)
	} else if (currentHour < 18) {
		return getString(properties.begroeting.middag)
	} else {
		return getString(properties.begroeting.avond)
	}
}

const getPersoonAanhef = (persoon: Persoon): string => {
	let aanhef = ""
	switch (persoon.geslacht) {
		case Geslacht.MAN:
			aanhef += "Meneer"
			break
		case Geslacht.VROUW:
			aanhef += "Mevrouw"
			break
		case Geslacht.ONBEKEND:
			aanhef += persoon.voorletters
			break
	}
	aanhef += " "
	switch (persoon.geslacht) {
		case Geslacht.MAN:
		case Geslacht.VROUW:
			aanhef += persoon.aanspreekTussenvoegselEnAchternaam.charAt(0).toUpperCase() + persoon.aanspreekTussenvoegselEnAchternaam.slice(1)
			break
		case Geslacht.ONBEKEND:
			aanhef += persoon.aanspreekTussenvoegselEnAchternaam
			break
	}
	return aanhef
}

export default LandingPage
