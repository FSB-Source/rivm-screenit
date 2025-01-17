/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import properties from "./ColonTopTakenComponent.json"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import {getHuidigeHuisarts, getVorigeHuisarts} from "../../api/HuisartsThunkAction"
import {useThunkDispatch} from "../../index"
import {Bevolkingsonderzoek} from "../../datatypes/Bevolkingsonderzoek"
import HeraanmeldenIcon from "../../scss/media/icons_toptaken/HeraanmeldenIcon/HeraanmeldenIcon"
import FitAanvragenIcon from "../../scss/media/icons_toptaken/FitAanvragenIcon/FitAanvragenIcon"
import AfspraakIcon from "../../scss/media/icons_toptaken/AfspraakIcon/AfspraakIcon"
import HuisartsIcon from "../../scss/media/icons_toptaken/HuisartsIcon/HuisartsIcon"

export type ColonTopTakenComponentProps = {
	className?: string
	getTekstHuisartsToptaak: (huisartsHuidigeRondeIsBekend: boolean, huisartsVorigeRondeIsBekend: boolean) => "controleren" | "wijzigen" | "opgeven"
	beschikbareActies: ClientContactActieType[]
}

const ColonTopTakenComponent = (props: ColonTopTakenComponentProps) => {

	const dispatch = useThunkDispatch()
	const huisartsVorigeRondeIsBekend = useSelector((state: State) => !!state.client.colonDossier.huisartsVorigeRonde)
	const huisartsHuidigeRondeIsBekend = useSelector((state: State) => !!state.client.colonDossier.huisartsHuidigeRonde)
	const persoon = useSelector((state: State) => state.client.persoon)
	const magNieuweAfspraakMaken = useSelector((state: State) =>
		props.beschikbareActies.includes(ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN)
		&& (state.client.colonDossier.intakeAfspraak?.afspraakAfgezegd
			|| state.client.colonDossier.intakeAfspraak?.andereIntakelocatieOpVerzoekClient),
	)

	useEffect(() => {
		dispatch(getVorigeHuisarts(Bevolkingsonderzoek.COLON))
		dispatch(getHuidigeHuisarts(Bevolkingsonderzoek.COLON))
	}, [dispatch])

	return (
		<Row className={props.className}>
			{props.beschikbareActies.includes(ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN) && persoon.vertrokkenUitNederland === false && <Col lg={4}>
				<TopTaakComponent icon={<AfspraakIcon/>}
								  link="/colon/afspraak-wijzigen/"
								  titel={getString(properties.afspraak.verzetten)}/>
			</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN) && <Col lg={4}>
				<TopTaakComponent icon={<AfspraakIcon/>}
								  link="/colon/afzeggen/"
								  titel={getString(properties.afspraak.afzeggen)}/>
			</Col>}
			{magNieuweAfspraakMaken &&
				<Col lg={4}>
					<TopTaakComponent icon={<AfspraakIcon/>}
									  link="/colon/afspraak-maken/"
									  titel={getString(properties.afspraak.aanmaken)}/>
				</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.COLON_HERAANMELDEN) && <Col lg={4}>
				<TopTaakComponent icon={<HeraanmeldenIcon/>}
								  link="/colon/heraanmelden/"
								  titel={getString(properties.heraanmelden)}/>
			</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.COLON_HUISARTS_WIJZIGEN) && <Col lg={4}>
				<TopTaakComponent icon={<HuisartsIcon/>}
								  link="/colon/huisarts/"
								  titel={getString(properties.huisarts[props.getTekstHuisartsToptaak(huisartsHuidigeRondeIsBekend, huisartsVorigeRondeIsBekend)])}/>
			</Col>}
			{props.beschikbareActies.includes(ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT) && <Col lg={4}>
				<TopTaakComponent icon={<FitAanvragenIcon/>}
								  link="/colon/fit/"
								  titel={getString(properties.fit_aanvragen)}/>
			</Col>}
		</Row>
	)
}

export default ColonTopTakenComponent
