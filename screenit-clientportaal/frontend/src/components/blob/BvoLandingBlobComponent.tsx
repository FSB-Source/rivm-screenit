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
import React from "react"
import TextBlobComponent from "./TextBlobComponent"
import properties from "./BvoLandingBlobComponent.json"
import {getString} from "../../utils/TekstPropertyUtil"

export type BvoLandingBlobComponentProps = {
	afspraakMoment: string,
	afspraakLocatie: string,
	extraTekst?: string
}

const BvoLandingBlobComponent = (props: BvoLandingBlobComponentProps) => {
    return (
		<TextBlobComponent titel={getString(properties.afspraak.titel)}
						   tekst={getString(properties.afspraak.moment, [props.afspraakMoment])}
						   extraTekst={props.extraTekst}
						   adresLocatie={props.afspraakLocatie}/>
	)
}

export default BvoLandingBlobComponent
