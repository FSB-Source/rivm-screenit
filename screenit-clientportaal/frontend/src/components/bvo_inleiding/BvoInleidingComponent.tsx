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
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../datatypes/Bevolkingsonderzoek"
import React from "react"
import {useSelectedBvo} from "../../utils/Hooks"
import InleidingComponent from "./InleidingComponent"
import {getBevolkingsonderzoekNederlandUrl} from "../../utils/UrlUtil"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import properties from "./BvoInleidingComponent.json"
import {getString} from "../../utils/TekstPropertyUtil"
import {assertUnreachable} from "../../utils/EnumUtil"

const BvoInleidingComponent = () => {

    const bvo = useSelectedBvo()!

    const toonVervangendeTekstCervix: boolean = useSelector((state: State) => state.landingOverzicht.cervixParameters?.toonVervangendeTekst)
    const toonVervangendeTekstMamma: boolean = useSelector((state: State) => state.landingOverzicht.mammaParameters?.toonVervangendeTekst)
    const toonVervangendeTekstColon: boolean = useSelector((state: State) => state.landingOverzicht.colonParameters?.toonVervangendeTekst)

    const vervangendeTekstCervix: string = useSelector((state: State) => state.landingOverzicht.cervixParameters?.vervangendeTekst)
    const vervangendeTekstMamma: string = useSelector((state: State) => state.landingOverzicht.mammaParameters?.vervangendeTekst)
    const vervangendeTekstColon: string = useSelector((state: State) => state.landingOverzicht.colonParameters?.vervangendeTekst)

    function getInleidingBvoTekst(): string {
        switch (bvo) {
            case Bevolkingsonderzoek.CERVIX:
                return toonVervangendeTekstCervix ? vervangendeTekstCervix : getString(properties.inleidingtekst.cervix)
            case Bevolkingsonderzoek.MAMMA:
                return toonVervangendeTekstMamma ? vervangendeTekstMamma : getString(properties.inleidingtekst.mamma)
            case Bevolkingsonderzoek.COLON:
                return toonVervangendeTekstColon ? vervangendeTekstColon : getString(properties.inleidingtekst.colon)
            default:
                assertUnreachable(bvo)

        }
    }

    const tijdelijkeMeldingCervix = useSelector((state: State) => state.landingOverzicht.cervixParameters?.tijdelijkeMelding)
    const tijdelijkeMeldingMamma = useSelector((state: State) => state.landingOverzicht.mammaParameters?.tijdelijkeMelding)
    const tijdelijkeMeldingColon = useSelector((state: State) => state.landingOverzicht.colonParameters?.tijdelijkeMelding)

    const tijdelijkeMelding = getTijdelijkeMelding()

    function getTijdelijkeMelding(): string {
        switch (bvo) {
            case Bevolkingsonderzoek.CERVIX:
                return tijdelijkeMeldingCervix
            case Bevolkingsonderzoek.MAMMA:
                return tijdelijkeMeldingMamma
            case Bevolkingsonderzoek.COLON:
                return tijdelijkeMeldingColon
            default:
                assertUnreachable(bvo)
        }
    }

    const toonStandaardTekst = (bvo === Bevolkingsonderzoek.MAMMA && !toonVervangendeTekstMamma) || (bvo === Bevolkingsonderzoek.CERVIX && !toonVervangendeTekstCervix) || (bvo === Bevolkingsonderzoek.COLON && !toonVervangendeTekstColon)
    const link = toonStandaardTekst ? getBevolkingsonderzoekNederlandUrl() + "/" + BevolkingsonderzoekNaam[bvo] : ""
    const linkTekst = toonStandaardTekst ? getString(properties.linkTekst, [BevolkingsonderzoekNaam[bvo]]) : ""

    return <InleidingComponent bvoNaam={""}
                               groteTitel={BevolkingsonderzoekNaam[bvo!]}
                               inleidingBvoTekst={getInleidingBvoTekst()}
                               hintTekst={tijdelijkeMelding}
                               link={link}
                               linkTekst={linkTekst}
                               toonAlgemeneInleidingTekst={toonStandaardTekst}/>
};

export default BvoInleidingComponent
