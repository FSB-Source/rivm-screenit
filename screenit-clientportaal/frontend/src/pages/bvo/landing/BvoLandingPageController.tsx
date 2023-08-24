/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {useSelectedBvo} from "../../../utils/Hooks"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import {Bevolkingsonderzoek} from "../../../datatypes/Bevolkingsonderzoek"
import MammaLandingPage from "./MammaLandingPage"
import CervixLandingPage from "./CervixLandingPage"
import ColonLandingPage from "./ColonLandingPage"
import {assertUnreachable} from "../../../utils/EnumUtil"
import LadenComponent from "../../../components/laden/LadenComponent"

const BvoLandingPageController = () => {
    const bvo = useSelectedBvo()
    const client = useSelector((state: State) => state.client)
    const beschikbareActies = client.beschikbareActies.beschikbareActies

    useEffect(() => {
        window.scrollTo(0, 0)
    }, [])

    switch (bvo) {
        case Bevolkingsonderzoek.CERVIX:
            return  client.cervixDossier.isInSync ? <CervixLandingPage dossier={client.cervixDossier} beschikbareActies={beschikbareActies}/> : <LadenComponent/>
        case  Bevolkingsonderzoek.COLON:
            return client.colonDossier.isInSync ? <ColonLandingPage dossier={client.colonDossier} beschikbareActies={beschikbareActies}/> : <LadenComponent/>
        case Bevolkingsonderzoek.MAMMA:
            return client.mammaDossier.isInSync ? <MammaLandingPage dossier={client.mammaDossier} beschikbareActies={beschikbareActies}/> : <LadenComponent/>
        default:
            assertUnreachable()
    }
}

export default BvoLandingPageController
