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
import React, {useEffect, useState} from "react"
import {Col, Row} from "react-bootstrap"
import styles from "./BvoTakenComponent.module.scss"
import TaakComponent from "../taken/TaakComponent"
import {useSelectedBvo} from "../../utils/Hooks"
import {getString} from "../../utils/TekstPropertyUtil"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import {Bevolkingsonderzoek} from "../../datatypes/Bevolkingsonderzoek"
import {getLaatsteBezwaarMoment} from "../../api/BezwaarThunkAction"
import {setLaatsteBezwaarMomentAction} from "../../actions/BezwaarReduxAction"
import {useThunkDispatch} from "../../index"
import {AfmeldType} from "../../datatypes/afmelden/AfmeldType"
import {ClientContactActieType} from "../../datatypes/ClientContactActieType"
import {getAfmeldenUrl, getBezwaarUrl} from "../../utils/UrlUtil"
import HuisIcon from "../../scss/media/icons_toptaken/AdresWijzigenIcon/HuisIcon"
import AfmeldenIcon from "../../scss/media/icons_toptaken/AfmeldenOnderzoekIcon/AfmeldenOnderzoekIcon"
import BezwaarIcon from "../../scss/media/icons_toptaken/BezwaarMakenIcon/BezwaarMakenIcon"
import TelefoonnummerIcon from "../../scss/media/icons_toptaken/TelefoonnummerWijzigenIcon/TelefoonnummerWijzigenIcon"
import ScreenitBackend from "../../utils/Backend"
import {AfmeldOptiesDto, geenAfmeldOpties} from "../../datatypes/afmelden/AfmeldOptiesDto"
import properties from "./BvoTakenComponent.json"

export type BvoTakenComponentProps = {
    beschikbareActies: ClientContactActieType[],
    toonVervangendeTekst: boolean;
}

const BvoTakenComponent = (props: BvoTakenComponentProps) => {
    const bvo = useSelectedBvo()!
    const dispatch = useThunkDispatch()
    const persoon = useSelector((state: State) => state.client.persoon)
    const beschikbareActies = props.beschikbareActies
    const bezwaren = useSelector((state: State) => state.client.laatsteBezwaarMoment)
    const [afmeldOpties, setAfmeldOpties] = useState<AfmeldOptiesDto>(geenAfmeldOpties)

    const afmeldContactActieTypeVanBvo: ClientContactActieType = bvo === Bevolkingsonderzoek.MAMMA ? ClientContactActieType.MAMMA_AFMELDEN : bvo === Bevolkingsonderzoek.CERVIX ? ClientContactActieType.CERVIX_AFMELDEN : ClientContactActieType.COLON_AFMELDEN

    useEffect(() => {
        ScreenitBackend.get(`/afmelden/${bvo}`)
            .then((response) => {
                setAfmeldOpties(response.data)
            })

        bvo && dispatch(getLaatsteBezwaarMoment(bvo))
        return () => {
            dispatch(setLaatsteBezwaarMomentAction([]))
        }
    }, [bvo, dispatch])

    return (
		<Row className={styles.bvoSelectie}>
			<Col lg={4}>
				<TaakComponent icon={<TelefoonnummerIcon/>}
							   link="/profiel/telefoonnummer"
							   tekst={(persoon.telefoonnummer1 || persoon.telefoonnummer2) ? getString(properties.taak.telefoonnummer.wijzigen) : getString(properties.taak.telefoonnummer.doorgeven)}/>
			</Col>
			{beschikbareActies.includes(afmeldContactActieTypeVanBvo) &&
			<Col lg={4}>
				<TaakComponent icon={<AfmeldenIcon/>}
							   link={getAfmeldenUrl(bvo)}
							   tekst={afmeldOpties.afmeldOpties.includes(AfmeldType.EENMALIG) ? getString(properties.taak.afmelden.algemeen) : getString(properties.taak.afmelden.definitief)}/>
			</Col>
			}
			{!props.toonVervangendeTekst && beschikbareActies.includes(ClientContactActieType.BEZWAAR) &&
			<Col lg={4}>
				<TaakComponent icon={<BezwaarIcon/>}
							   link={getBezwaarUrl(bvo)}
							   tekst={bezwaren.find(b => b.active) ? getString(properties.taak.bezwaar.intrekken) : getString(properties.taak.bezwaar.maken)}/>
			</Col>
			}
			{beschikbareActies.includes(ClientContactActieType.TIJDELIJK_ADRES) &&
			<Col lg={4}>
				<TaakComponent icon={<HuisIcon/>}
							   link="/profiel/adres"
							   tekst={persoon.tijdelijkAdres ? getString(properties.taak.adres.wijzigen) : getString(properties.taak.adres.doorgeven)}/>
			</Col>
			}
		</Row>
	)

}

export default BvoTakenComponent
