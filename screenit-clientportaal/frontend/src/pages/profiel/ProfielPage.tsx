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
import styles from "./ProfielPage.module.scss"
import {Col, Container, Row} from "react-bootstrap"
import TopTaakComponent from "../../components/taken/TopTaakComponent"
import classNames from "classnames"
import InleidingComponent from "../../components/bvo_inleiding/InleidingComponent"
import BvoSelectieComponent from "../../components/bvo_selectie/BvoSelectieComponent"
import KruimelpadComponent from "../../components/kruimelpad/KruimelpadComponent"
import {getString} from "../../utils/TekstPropertyUtil"
import {heeftAanspreekVorm, heeftTelefoonnummer, Persoon} from "../../datatypes/Persoon"
import {formatDateText, isDatumVandaagOfLater} from "../../utils/DateUtil"
import {getAdresStringMetHtmlSeparator} from "../../utils/AdresUtil"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import {isNullOfLeeg, isNullOfUndefined} from "../../utils/EmptyUtil"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import TelefoonnummerIcon from "../../scss/media/icons_toptaken/TelefoonnummerWijzigenIcon/TelefoonnummerWijzigenIcon"
import EmailadresWijzigenIcon from "../../scss/media/icons_toptaken/EmailadresWijzigenIcon/EmailadresWijzigenIcon"
import HuisIcon from "../../scss/media/icons_toptaken/AdresWijzigenIcon/HuisIcon"
import properties from "./ProfielPage.json"
import {ClientContactActieType} from "../../datatypes/ClientContactActieType"
import {TijdelijkAdres} from "../../datatypes/adres/TijdelijkAdres"
import {AanhefType} from "../../datatypes/aanhef/AanhefType"
import AanspreekVormIcon from "../../scss/media/icons_toptaken/AanspreekVormIcon/AanspreekVormIcon"

const ProfielPage = () => {
	const persoon = useSelector((state: State) => state.client.persoon)
	const toonTijdelijkAdres = !isNullOfLeeg(persoon.tijdelijkAdresTekst) && (isNullOfUndefined(persoon.tijdelijkAdres?.eindDatum) || isDatumVandaagOfLater(persoon.tijdelijkAdres!.eindDatum!))
	const beschikbareActies = useSelector((state: State) => state.client.beschikbareActies.beschikbareActies)

	return (
		<Container fluid className={classNames(styles.content)}>
			<KruimelpadComponent/>
			<Row>
				<Col xl={8}>
					<InleidingComponent bvoNaam={""}
										groteTitel={getString(properties.page.title)}
										inleidingBvoTekst={getString(properties.page.description)}
										toonAlgemeneInleidingTekst={false}/>
				</Col>
				<Col xl={4}>
					<div className={styles.paspoort}>
						<h6>{getString(properties.paspoort.title)}</h6>
						<h3>{persoon.voorletters} {persoon.aanspreekTussenvoegselEnAchternaam}</h3>
						<span
							className={styles.lichtGrijs}>{getString(properties.paspoort.label.geboortedatum)} {persoon.geboortedatumDisplay}</span>
						<span
							className={styles.lichtGrijs}>{getString(properties.paspoort.label.bsn)} {persoon.bsn}</span>
						<h6 className={styles.adres}>{getString(properties.paspoort.label.adres)}</h6>
						<SpanWithHtml value={getAdresStringMetHtmlSeparator(persoon.adresTekst)}/>
					</div>
				</Col>
			</Row>
			{!beschikbareActies.includes(ClientContactActieType.GEEN) && persoon.vertrokkenUitNederland === false &&
				<Row className={styles.topTaakSelectie}>
					<Col lg={4}>
						<TopTaakComponent icon={<TelefoonnummerIcon/>}
										  link="/profiel/telefoonnummer/"
										  titel={getString(properties.toptaak.title.telefoonnummer)}
										  subTitel={heeftTelefoonnummer(persoon) ? getString(properties.toptaak.subtitle.telefoonnummer) : ""}
										  subTekst={heeftTelefoonnummer(persoon) ? (persoon.telefoonnummer1 ? persoon.telefoonnummer1 + "<br/>" : "") + (persoon.telefoonnummer2 ? persoon.telefoonnummer2 : "") : ""}/>
					</Col>
					<Col lg={4}>
						<TopTaakComponent icon={<EmailadresWijzigenIcon/>}
										  link="/profiel/email/"
										  titel={getString(properties.toptaak.title.email)}
						/>
					</Col>
					<Col lg={4}>
						<TopTaakComponent icon={<HuisIcon/>}
										  link="/profiel/adres/"
										  titel={getString(toonTijdelijkAdres ? properties.toptaak.title.adres.wijzigen : properties.toptaak.title.adres.opgeven)}
										  subTitel={toonTijdelijkAdres ? getDatumVermeldingTijdelijkAdres(persoon.tijdelijkAdres!) : ""}
										  subTekst={toonTijdelijkAdres ? getAdresStringMetHtmlSeparator(persoon.tijdelijkAdresTekst) : ""}/>
					</Col>
					<Col lg={4}>
						<TopTaakComponent icon={<AanspreekVormIcon/>}
										  link="/profiel/aanspreekvorm/"
										  titel={getString(properties.toptaak.title.aanspreekvorm)}
										  subTitel={heeftAanspreekVorm(persoon) ? getString(properties.toptaak.subtitle.aanspreekvorm) : ""}
										  subTekst={heeftAanspreekVorm(persoon) ? getAanhef(persoon) : ""}/>
					</Col>
				</Row>
			}
			<div className={styles.bvoSelectie}>
				<h5>Mijn onderzoeken</h5>
				<BvoSelectieComponent/>
			</div>
		</Container>
	)

	function getDatumVermeldingTijdelijkAdres(tijdelijkAdres: TijdelijkAdres): string {
		if (isDatumVandaagOfLater(tijdelijkAdres.startDatum)) {
			return getString(properties.toptaak.subtitle.adres, ["vanaf " + formatDateText(tijdelijkAdres.startDatum), tijdelijkAdres.eindDatum ? "t/m " + formatDateText(tijdelijkAdres.eindDatum) : "voor onbepaalde tijd"])
		} else if (isNullOfUndefined(tijdelijkAdres.eindDatum)) {
			return getString(properties.toptaak.subtitle.adres, ["", "voor onbepaalde tijd"])
		} else {
			return getString(properties.toptaak.subtitle.adres, ["", "t/m " + formatDateText(tijdelijkAdres.eindDatum)])
		}
	}

	function getAanhef(persoon: Persoon): string {
		if (persoon.aanhef === AanhefType.GEACHTE_MEVROUW) {
			return getString(properties.toptaak.subtitle.aanspreekvormen.geachte_mevrouw, [persoon.aanspreekTussenvoegselEnAchternaam])
		} else if (persoon.aanhef === AanhefType.GEACHTE_HEER) {
			return getString(properties.toptaak.subtitle.aanspreekvormen.geachte_heer, [persoon.aanspreekTussenvoegselEnAchternaam])
		} else {
			return getString(properties.toptaak.subtitle.aanspreekvormen.geachte, [persoon.voorletters, persoon.aanspreekTussenvoegselEnAchternaam])
		}
	}

}

export default ProfielPage
