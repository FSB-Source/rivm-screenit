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
import {Col, Row} from "react-bootstrap"
import TopTaakComponent from "../taken/TopTaakComponent"
import {ClientContactActieType} from "../../datatypes/ClientContactActieType"
import {getString} from "../../utils/TekstPropertyUtil"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import {getHuidigeHuisarts, getHuidigeMammaGeenHuisartsOptie, getVorigeHuisarts, getVorigeMammaGeenHuisartsOptie} from "../../api/HuisartsThunkAction"
import {useThunkDispatch} from "../../index"
import {Bevolkingsonderzoek} from "../../datatypes/Bevolkingsonderzoek"
import {getHuidigeAfspraak} from "../../api/MammaAfspraakMakenThunkAction"
import HeraanmeldenIcon from "../../scss/media/icons_toptaken/HeraanmeldenIcon/HeraanmeldenIcon"
import AfspraakIcon from "../../scss/media/icons_toptaken/AfspraakIcon/AfspraakIcon"
import HuisartsIcon from "../../scss/media/icons_toptaken/HuisartsIcon/HuisartsIcon"
import properties from "./MammaTopTakenComponent.json"

export type MammaTopTakenComponentProps = {
    className?: string
    getTekstHuisartsToptaak: (huisartsHuidigeRondeIsBekend: boolean, huisartsVorigeRondeIsBekend: boolean) => "controleren" | "wijzigen" | "opgeven"
    beschikbareActies: ClientContactActieType[]
}

const MammaTopTakenComponent = (props: MammaTopTakenComponentProps) => {

    const dispatch = useThunkDispatch()

    const huisartsVorigeRondeIsBekend = useSelector((state: State) => !!state.client.mammaDossier.huisartsVorigeRonde)
    const geenHuisartsOptieHuidigeRonde = useSelector((state:State) => !!state.client.mammaDossier.geenHuisartsOptieHuidigeRonde)
    const huisartsHuidigeRondeIsBekend = useSelector((state: State) => !!state.client.mammaDossier.huisartsHuidigeRonde)
    const geenHuisartsOptieVorigeRonde = useSelector((state:State) => !!state.client.mammaDossier.geenHuisartsOptieVorigeRonde)

    const huidigeAfspraak = useSelector((state: State) => state.client.mammaDossier.huidigeAfspraak)

    useEffect(() => {
        dispatch(getVorigeHuisarts(Bevolkingsonderzoek.MAMMA))
        dispatch(getHuidigeHuisarts(Bevolkingsonderzoek.MAMMA))
        dispatch(getVorigeMammaGeenHuisartsOptie(Bevolkingsonderzoek.MAMMA))
        dispatch(getHuidigeMammaGeenHuisartsOptie(Bevolkingsonderzoek.MAMMA))
        dispatch(getHuidigeAfspraak())
    }, [dispatch])

    return (
        <Row className={props.className}>
            {props.beschikbareActies && (props.beschikbareActies.includes(ClientContactActieType.MAMMA_AFSPRAAK_MAKEN) || props.beschikbareActies.includes(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN)) &&
			<Col lg={4}>
				<TopTaakComponent icon={<AfspraakIcon/>}
								  link="/mamma/afspraak/"
								  titel={props.beschikbareActies.includes(ClientContactActieType.MAMMA_AFSPRAAK_MAKEN) ? getString(properties.afspraak.maken) : getString(properties.afspraak.verzetten)}/>
			</Col>}
            {props.beschikbareActies && huidigeAfspraak && props.beschikbareActies.includes(ClientContactActieType.MAMMA_AFMELDEN) &&
			<Col lg={4}>
				<TopTaakComponent icon={<AfspraakIcon/>}
								  link="/mamma/afmelden/"
								  titel={getString(properties.afspraak.afzeggen)}/>
			</Col>}
            {props.beschikbareActies && props.beschikbareActies.includes(ClientContactActieType.MAMMA_HERAANMELDEN) &&
			<Col lg={4}>
				<TopTaakComponent icon={<HeraanmeldenIcon/>}
								  link="/mamma/heraanmelden/"
								  titel={getString(properties.heraanmelden)}/>
			</Col>}
            {props.beschikbareActies && props.beschikbareActies.includes(ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN) &&
			<Col lg={4}>
				<TopTaakComponent icon={<HuisartsIcon/>}
								  link="/mamma/huisarts/"
								  titel={getString(properties.huisarts[props.getTekstHuisartsToptaak(huisartsHuidigeRondeIsBekend || geenHuisartsOptieHuidigeRonde, huisartsVorigeRondeIsBekend || geenHuisartsOptieVorigeRonde)])}/>
			</Col>}
        </Row>
    )
}

export default MammaTopTakenComponent
