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
import React from "react"
import bvoStyle from "../../BvoStyle.module.scss"
import {ArrowType} from "../../vectors/ArrowIconComponent"
import SubmitButton from "../../input/SubmitButton"
import {useNavigate} from "react-router-dom"
import {saveAfspraakAfzeggen} from "../../../api/ColonAfspraakAfzeggenThunkAction"
import {useThunkDispatch} from "../../../index"
import {showToast} from "../../../utils/ToastUtil"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "../../../pages/bvo/colon/ColonAfspraakAfzeggenPage.json"

const ColonAfspraakAfzeggenForm = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()

	const afspraakAfzeggen = () => {
		dispatch(saveAfspraakAfzeggen).then(() => {
			showToast(getString(properties.toast.title), getString(properties.toast.description))
			navigate("/colon")
		})
	}

	return <>
		<SubmitButton className={bvoStyle.baseBackgroundColor}
					  label={"Afspraak afzeggen"}
					  displayArrow={ArrowType.ARROW_RIGHT}
					  onClick={() => {
						  afspraakAfzeggen()
					  }}/>
	</>
}

export default ColonAfspraakAfzeggenForm
