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
import React, {useEffect} from "react"
import {Col, Container, Row} from "react-bootstrap"
import styles from "./BvoLandingPage.module.scss"
import landingPageStyle from "../../landing/LandingPage.module.scss"
import BvoSelectieComponent from "../../../components/bvo_selectie/BvoSelectieComponent"
import BvoHistorieComponent from "../../../components/bvo_historie/BvoHistorieComponent"
import KruimelpadComponent from "../../../components/kruimelpad/KruimelpadComponent"
import BvoInleidingComponent from "../../../components/bvo_inleiding/BvoInleidingComponent"
import BvoTakenComponent from "../../../components/bvo_acties/BvoTakenComponent"
import ColonTopTakenComponent from "../../../components/bvo_acties/ColonTopTakenComponent"
import ImageBlobComponent from "../../../components/blob/ImageBlobComponent"
import blob_personen from "../../../scss/media/blob-personen.jpg"
import {ColonDossier} from "../../../datatypes/ColonDossier"
import {ClientContactActieType} from "../../../datatypes/ClientContactActieType"
import bvoStyles from "../../../components/BvoStyle.module.scss"
import {useSelector} from "react-redux"
import {getHuidigeIntakeAfspraak} from "../../../api/ColonAfspraakAfzeggenThunkAction"
import BvoLandingBlobComponent from "../../../components/blob/BvoLandingBlobComponent"
import {splitAdresString} from "../../../utils/StringUtil"
import {State} from "../../../datatypes/State"
import {useThunkDispatch} from "../../../index"

type Props = {
	dossier: ColonDossier,
	beschikbareActies: ClientContactActieType[]
}

const ColonLandingPage = (props: Props) => {
	const {dossier, beschikbareActies} = props

	const dispatch = useThunkDispatch()
	const locatieIntakeAfspraak = dossier.intakeAfspraak ? dossier.intakeAfspraak.naamInstelling + "<br>" + splitAdresString(dossier.intakeAfspraak.adresString) : ""
	const toonVervangendeTekst: boolean = useSelector((state: State) => state.landingOverzicht.colonParameters.toonVervangendeTekst)

	useEffect(() => {
		dispatch(getHuidigeIntakeAfspraak())
	}, [dispatch])

	return (
		<Container fluid className={styles.content}>
			<KruimelpadComponent className={bvoStyles.colon}/>
			<Row className={landingPageStyle.inleiding}>
				<Col md={8}>
					<BvoInleidingComponent/>
				</Col>
				<Col md={4}>
					{dossier.intakeAfspraak && !dossier.intakeAfspraak.afspraakAfgezegd && !toonVervangendeTekst ?
						<BvoLandingBlobComponent afspraakMoment={dossier.intakeAfspraak.weergaveAfspraakmoment}
												 afspraakLocatie={locatieIntakeAfspraak}/>
						: <ImageBlobComponent image={blob_personen}/>}
				</Col>
			</Row>
			{!beschikbareActies.includes(ClientContactActieType.GEEN) &&
				<ColonTopTakenComponent className={styles.topTaak}
										beschikbareActies={beschikbareActies}
										getTekstHuisartsToptaak={getHuisartsTekst}/>
			}

			{!beschikbareActies.includes(ClientContactActieType.GEEN) &&
				<BvoTakenComponent beschikbareActies={beschikbareActies}
								   toonVervangendeTekst={toonVervangendeTekst}/>
			}

			{!toonVervangendeTekst && <BvoHistorieComponent gebeurtenissen={props.dossier.gebeurtenissenLaatsteRonde}/>}
			<h5 className={landingPageStyle.sectieHeader}>Mijn onderzoeken</h5>
			<BvoSelectieComponent/>
		</Container>
	)

	function getHuisartsTekst(huisartsHuidigeRondeIsBekend: boolean, huisartsVorigeRondeIsBekend: boolean): "controleren" | "wijzigen" | "opgeven" {
		if (huisartsVorigeRondeIsBekend && !huisartsHuidigeRondeIsBekend) {
			return "controleren"
		}
		if (huisartsHuidigeRondeIsBekend) {
			return "wijzigen"
		} else {
			return "opgeven"
		}
	}

}

export default ColonLandingPage
