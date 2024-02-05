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
import React from "react"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import ActieBasePage from "../../ActieBasePage"
import classNames from "classnames"
import bvoStyle from "../../../components/BvoStyle.module.scss"
import {saveNieuwHerdrukAanvraag} from "../../../api/HerdrukkenThunkAction"
import {useThunkDispatch} from "../../../index"
import {getString} from "../../../utils/TekstPropertyUtil"
import {ArrowType} from "../../../components/vectors/ArrowIconComponent"
import properties from "./CervixHerdrukAanvragenPage.json"
import SubmitButton from "../../../components/input/SubmitButton"
import {useNavigate} from "react-router-dom"
import {showToast} from "../../../utils/ToastUtil"

const CervixHerdrukAanvragenPage = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[Bevolkingsonderzoek.CERVIX]}
			title={getString(properties.title)}
			description={getString(properties.description)}>

			<SubmitButton
				className={classNames(bvoStyle.baseBackgroundColor)}
				label={getString(properties.button)}
				displayArrow={ArrowType.ARROW_RIGHT}
				onClick={() => {
					dispatch(saveNieuwHerdrukAanvraag())
						.then(() => {
							showToast(getString(properties.toast.title), getString(properties.toast.description))
							navigate("/cervix")
						})
				}}/>

		</ActieBasePage>
	)

}

export default CervixHerdrukAanvragenPage
