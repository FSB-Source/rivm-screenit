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
import {Col, Row} from "react-bootstrap"
import styles from "./BvoSelectieComponent.module.scss"
import BvoCard from "../bvo_card/BvoCard"
import {Bevolkingsonderzoek} from "../../datatypes/Bevolkingsonderzoek"
import React from "react"
import classNames from "classnames"
import {Geslacht} from "../../datatypes/Geslacht"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import {navigate} from "../../routes/routes"

type BvoSelectieComponentProps = {
    className?: string
}

const BvoSelectieComponent = (props: BvoSelectieComponentProps) => {

    const persoon = useSelector((state: State) => state.client.persoon)

    const clientBehoortTotMammaDoelgroep = useSelector((state: State) => state.landingOverzicht.clientBehoortTotMammaDoelgroep)
    const clientBehoortTotColonDoelgroep = useSelector((state: State) => state.landingOverzicht.clientBehoortTotColonDoelgroep)
    const clientBehoortTotCervixDoelgroep = useSelector((state: State) => state.landingOverzicht.clientBehoortTotCervixDoelgroep)

    return (
        <Row className={classNames(styles.bvoSelectie, props.className)}>
            {persoon.geslacht !== Geslacht.MAN &&
            <>
                <Col lg={4}>
                    <BvoCard clientValtInDoelgroep={clientBehoortTotMammaDoelgroep} bvo={Bevolkingsonderzoek.MAMMA}
                             onClick={() => navigate("/mamma")}/>
                </Col>
                <Col lg={4}>
                    <BvoCard clientValtInDoelgroep={clientBehoortTotCervixDoelgroep} bvo={Bevolkingsonderzoek.CERVIX}
                             onClick={() => navigate("/cervix")}/>
                </Col>
            </>
            }
            <Col lg={4}>
                <BvoCard clientValtInDoelgroep={clientBehoortTotColonDoelgroep} bvo={Bevolkingsonderzoek.COLON}
                         onClick={() => navigate("/colon")}/>
            </Col>
        </Row>
    )

}

export default BvoSelectieComponent
