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
import {Col, Row} from "react-bootstrap"
import TopTaakComponent from "../taken/TopTaakComponent"
import {ClientContactActieType} from "../../datatypes/ClientContactActieType"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "./CervixTopTakenComponent.json"
import ZASAanvragenIcon from "../../scss/media/icons_toptaken/ZasAanvragenIcon/ZasAanvragenIcon"
import HeraanmeldenIcon from "../../scss/media/icons_toptaken/HeraanmeldenIcon/HeraanmeldenIcon"
import UitstellenIcon from "../../scss/media/icons_toptaken/UitstellenIcon/UitstellenIcon"
import HerdrukkenIcon from "../../scss/media/icons_toptaken/HerdrukkenIcon/HerdrukkenIcon"

export type CervixTopTakenComponentProps = {
    className?: string
    beschikbareActies: ClientContactActieType[]
}

const CervixTopTakenComponent = (props: CervixTopTakenComponentProps) => {

    return (
        <Row className={props.className}>
            {props.beschikbareActies.includes(ClientContactActieType.CERVIX_ZAS_AANVRAGEN) && <Col lg={4}>
				<TopTaakComponent icon={<ZASAanvragenIcon/>}
								  link="/cervix/zas"
								  titel={getString(properties.zas_aanvragen)}/>
			</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.CERVIX_HERAANMELDEN) && <Col lg={4}>
				<TopTaakComponent icon={<HeraanmeldenIcon/>}
								  link="/cervix/heraanmelden"
								  titel={getString(properties.heraanmelden)}/>
			</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.CERVIX_HERDRUK) && <Col lg={4}>
				<TopTaakComponent icon={<HerdrukkenIcon/>}
								  link="/cervix/herdrukken"
								  titel={getString(properties.herdrukken)}/>
			</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.CERVIX_UITSTEL) && <Col lg={4}>
				<TopTaakComponent icon={<UitstellenIcon/>}
								  link="/cervix/uitstellen"
								  titel={getString(properties.uitstellen)}/>
			</Col>}
        </Row>
    )

}

export default CervixTopTakenComponent
