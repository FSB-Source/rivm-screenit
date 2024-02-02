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
import {useThunkDispatch} from "../../../index"
import React, {useEffect} from "react"
import ActieBasePage from "../../ActieBasePage"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import {getString} from "../../../utils/TekstPropertyUtil"
import {getHuidigeIntakeAfspraak} from "../../../api/ColonAfspraakAfzeggenThunkAction"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import properties from "./ColonAfspraakAfzeggenPage.json"
import LadenComponent from "../../../components/laden/LadenComponent"
import {splitAdresString} from "../../../utils/StringUtil"
import ColonAfspraakAfzeggenForm from "../../../components/form/colon/ColonAfspraakAfzeggenForm"

const ColonAfspraakAfzeggenPage = () => {
	const dispatch = useThunkDispatch()
	const huidigeIntakeAfspraak = useSelector((state: State) => state.client.colonDossier.intakeAfspraak)

	useEffect(() => {
		dispatch(getHuidigeIntakeAfspraak())
	}, [dispatch])

	if (!huidigeIntakeAfspraak) {
		return <LadenComponent/>
	}

	return (
		<ActieBasePage bvoName={BevolkingsonderzoekNaam[Bevolkingsonderzoek.COLON]}
					   title={getString(properties.page.title)}
					   description={getString(properties.page.description)}
					   hintBegin={getString(properties.huidige_afspraak, [huidigeIntakeAfspraak.weergaveAfspraakmoment, huidigeIntakeAfspraak.naamInstelling, splitAdresString(huidigeIntakeAfspraak.adresString)])}>

			<ColonAfspraakAfzeggenForm/>

		</ActieBasePage>
	)

}

export default ColonAfspraakAfzeggenPage
