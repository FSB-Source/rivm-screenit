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
import {Col, Container, Row} from "react-bootstrap"
import styles from "./BasePage.module.scss"
import {useSelectedBvo} from "../utils/Hooks"
import classNames from "classnames"
import {BevolkingsonderzoekStyle} from "../datatypes/Bevolkingsonderzoek"
import InleidingComponent from "../components/bvo_inleiding/InleidingComponent"
import TextBlobComponent from "../components/blob/TextBlobComponent"
import KruimelpadComponent from "../components/kruimelpad/KruimelpadComponent"

export type BasePageProps = {
	bvoName: string,
	title: string,
	description: string,
	hint?: string,
	toonBlob: boolean,
	blobTitle?: string,
	blobText?: string,
	blobExtraText?: string,
	blobAdresLocatie?: string,
	onBlobLinkClick?: () => void,
	blobLinkText?: string,
	children: React.ReactNode,
}

const BasePage = (props: BasePageProps) => {

    const selectedBvo = useSelectedBvo()

    return (
        <Container fluid className={classNames(styles.content, selectedBvo && BevolkingsonderzoekStyle[selectedBvo])}>
            <KruimelpadComponent/>
            <Row>
                <Col md={8}>
                    <InleidingComponent bvoNaam={props.bvoName} groteTitel={props.title}
                                        inleidingBvoTekst={props.description}
                                        toonAlgemeneInleidingTekst={false}/>
                </Col>
                <Col md={4}>
					{props.toonBlob &&
						<TextBlobComponent
							titel={props.blobTitle ? props.blobTitle : ""}
							tekst={props.blobText ? props.blobText : ""}
							extraTekst={props.blobExtraText ? props.blobExtraText : ""}
							adresLocatie={props.blobAdresLocatie}
							onLinkClick={props.onBlobLinkClick}
							linkTekst={props.blobLinkText}/>}
                </Col>
            </Row>
            <div className={styles.childrenContainer}>
                {props.children}
            </div>
        </Container>
    )

}

export default BasePage
