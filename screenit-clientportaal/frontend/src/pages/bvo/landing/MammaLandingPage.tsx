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
import styles from "./BvoLandingPage.module.scss"
import bvoStyles from "../../../components/BvoStyle.module.scss"
import landingPageStyle from "../../landing/LandingPage.module.scss"
import BvoSelectieComponent from "../../../components/bvo_selectie/BvoSelectieComponent"
import BvoHistorieComponent from "../../../components/bvo_historie/BvoHistorieComponent"
import KruimelpadComponent from "../../../components/kruimelpad/KruimelpadComponent"
import BvoInleidingComponent from "../../../components/bvo_inleiding/BvoInleidingComponent"
import BvoTakenComponent from "../../../components/bvo_acties/BvoTakenComponent"
import {useDispatch, useSelector} from "react-redux"
import MammaTopTakenComponent from "../../../components/bvo_acties/MammaTopTakenComponent"
import {getHuidigeAfspraak} from "../../../api/MammaAfspraakMakenThunkAction"
import BvoLandingBlobComponent from "../../../components/blob/BvoLandingBlobComponent"
import {MammaDossier} from "../../../datatypes/MammaDossier"
import {ClientContactActieType} from "../../../datatypes/ClientContactActieType"
import {splitAdresString} from "../../../utils/StringUtil"
import ImageBlobComponent from "../../../components/blob/ImageBlobComponent"
import blob_mamma from "../../../scss/media/blob-mamma.jpg"
import {State} from "../../../datatypes/State"

type Props = {
    dossier: MammaDossier
    beschikbareActies: ClientContactActieType[]
}
const MammaLandingPage = (props: Props) => {

	const {dossier, beschikbareActies} = props

	const locatieHuidigeAfspraak = dossier.huidigeAfspraak ? splitAdresString(dossier.huidigeAfspraak.adresStandplaats) : ""
	const toonVervangendeTekst: boolean = useSelector((state: State) => state.landingOverzicht.mammaParameters.toonVervangendeTekst)

	const dispatch = useDispatch()

	useEffect(() => {
		dispatch(getHuidigeAfspraak())
	}, [dispatch])

	return (
		<Container fluid className={styles.content}>
			<KruimelpadComponent className={bvoStyles.mamma}/>
			<Row className={landingPageStyle.inleiding}>
				<Col md={8}>
					<BvoInleidingComponent/>
				</Col>
				<Col md={4}>
					{dossier.huidigeAfspraak && !toonVervangendeTekst ?
						<BvoLandingBlobComponent afspraakLocatie={locatieHuidigeAfspraak}
												 afspraakMoment={dossier.huidigeAfspraak.weergaveAfspraakMoment}/> :
						<ImageBlobComponent image={blob_mamma}/>}
				</Col>
			</Row>
			{!beschikbareActies.includes(ClientContactActieType.GEEN) &&
			<MammaTopTakenComponent className={styles.topTaak}
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

export default MammaLandingPage
