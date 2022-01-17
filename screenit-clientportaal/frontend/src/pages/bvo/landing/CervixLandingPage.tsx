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
import {Col, Container, Row} from "react-bootstrap"
import styles from "./BvoLandingPage.module.scss"
import landingPageStyle from "../../landing/LandingPage.module.scss"
import BvoSelectieComponent from "../../../components/bvo_selectie/BvoSelectieComponent"
import BvoHistorieComponent from "../../../components/bvo_historie/BvoHistorieComponent"
import KruimelpadComponent from "../../../components/kruimelpad/KruimelpadComponent"
import BvoInleidingComponent from "../../../components/bvo_inleiding/BvoInleidingComponent"
import BvoTakenComponent from "../../../components/bvo_acties/BvoTakenComponent"
import CervixTopTakenComponent from "../../../components/bvo_acties/CervixTopTakenComponent"
import {CervixDossier} from "../../../datatypes/CervixDossier"
import {ClientContactActieType} from "../../../datatypes/ClientContactActieType"
import bvoStyles from "../../../components/BvoStyle.module.scss"
import ImageBlobComponent from "../../../components/blob/ImageBlobComponent"
import blob_cervix from "../../../scss/media/blob-cervix.jpg"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"

type Props = {
    dossier: CervixDossier,
    beschikbareActies: ClientContactActieType[]
}
const CervixLandingPage = (props: Props) => {

    const beschikbareActies = props.beschikbareActies
    const toonVervangendeTekst: boolean = useSelector((state: State) => state.landingOverzicht.cervixParameters.toonVervangendeTekst)

    return (
        <Container fluid className={styles.content}>
            <KruimelpadComponent className={bvoStyles.cervix}/>
            <Row className={landingPageStyle.inleiding}>
                <Col md={8}>
                    <BvoInleidingComponent/>
                </Col>
                <Col md={4}>
                    <ImageBlobComponent image={blob_cervix}/>
                </Col>
            </Row>

            {!beschikbareActies.includes(ClientContactActieType.GEEN) &&
            <CervixTopTakenComponent className={styles.topTaak}
                                     beschikbareActies={beschikbareActies}/>
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
}

export default CervixLandingPage
