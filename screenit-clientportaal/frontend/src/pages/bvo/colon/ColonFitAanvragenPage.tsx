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
import ActieBasePage from "../../ActieBasePage"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import React, {useEffect} from "react"
import bvoStyle from "../../../components/BvoStyle.module.scss"
import {useThunkDispatch} from "../../../index"
import {getHuidigeFitStatus, saveFitAanvraag} from "../../../api/ColonFitThunkAction"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import {FormErrorComponent} from "../../../components/form_error/FormErrorComponent"
import {ArrowType} from "../../../components/vectors/ArrowIconComponent"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "./ColonFitAanvragenPage.json"
import SubmitButton from "../../../components/input/SubmitButton"
import {useNavigate} from "react-router-dom"

const ColonFitAanvragenPage = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const fitStatus = useSelector((state: State) => state.client.colonDossier.fitStatus)

	const aanvraagButtonZichtbaar: boolean = !fitStatus.maxAantalAanvragenBereikt

	useEffect(() => {
		dispatch(getHuidigeFitStatus())
	}, [dispatch])

	return (
		<ActieBasePage bvoName={BevolkingsonderzoekNaam[Bevolkingsonderzoek.COLON]}
					   title={getString(properties.page.title)}
					   description={getString(properties.page.description)}>

			{getMaxAantalFitAanvragenError()}

			{aanvraagButtonZichtbaar &&
				<SubmitButton className={bvoStyle.baseBackgroundColor}
							  label={getString(properties.button)}
							  displayArrow={ArrowType.ARROW_RIGHT}
							  onClick={() => {
								  dispatch(saveFitAanvraag()).then(() => navigate("/colon"))
							  }}/>}

		</ActieBasePage>
	)

	function getMaxAantalFitAanvragenError() {
		return fitStatus.maxAantalAanvragenBereikt &&
			<FormErrorComponent
				text={getString(properties.error.max_aantal)}/>
	}

}

export default ColonFitAanvragenPage
