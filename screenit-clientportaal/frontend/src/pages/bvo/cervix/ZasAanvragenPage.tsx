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
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import {FormErrorComponent} from "../../../components/form_error/FormErrorComponent"
import styles from "./ZasAanvragenPage.module.scss"
import ActieBasePage from "../../ActieBasePage"
import {getHuidigeZasStatus, saveZasAanvraag, saveZasAanvraagMetUitstel} from "../../../api/ZasThunkAction"
import {useThunkDispatch} from "../../../index"
import classNames from "classnames"
import bvoStyle from "../../../components/BvoStyle.module.scss"
import {getString} from "../../../utils/TekstPropertyUtil"
import ZasNaUitstelForm from "../../../components/form/cervix/ZasNaUitstelForm"
import properties from "./ZasAanvragenPage.json"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import {ArrowType} from "../../../components/vectors/ArrowIconComponent"
import LadenComponent from "../../../components/laden/LadenComponent"
import SubmitButton from "../../../components/input/SubmitButton"
import {useNavigate} from "react-router-dom"

const ZasAanvragenPage = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const zasStatus = useSelector((state: State) => state.client.cervixDossier.zasStatus)
	const formUitstelZichtbaar = !!zasStatus.uitstellenTotDatum && !zasStatus.maxAantalAanvragenBereikt
	const aanvraagButtonZichtbaar = !zasStatus.maxAantalAanvragenBereikt && !formUitstelZichtbaar

	useEffect(() => {
		dispatch(getHuidigeZasStatus())
	}, [dispatch])

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[Bevolkingsonderzoek.CERVIX]}
			title={getString(properties.inleiding.title)}
			description={formUitstelZichtbaar ? getString(properties.inleiding.description.na_uitstel) : getString(properties.inleiding.description.algemeen)}>

			{!zasStatus.isInSync && <LadenComponent/>}

			{zasStatus.isInSync && getMaxAantalZassenAangevraagdError()}

			{zasStatus.isInSync && formUitstelZichtbaar && <ZasNaUitstelForm uitstellenTotDatum={zasStatus.uitstellenTotDatum}
																			 onSubmitSucces={(zasNaUitstel) => {
																				 dispatch(saveZasAanvraagMetUitstel(zasNaUitstel, zasStatus.uitstellenTotDatum)).then(() => navigate("/cervix"))
																			 }}/>}

			{zasStatus.isInSync && aanvraagButtonZichtbaar &&
				<SubmitButton className={classNames(bvoStyle.baseBackgroundColor, styles.submitButton)}
							  label={getString(properties.form.button)}
							  displayArrow={ArrowType.ARROW_RIGHT}
							  onClick={() => {
								  dispatch(saveZasAanvraag()).then(() => navigate("/cervix"))
							  }}/>}

		</ActieBasePage>
	)

	function getMaxAantalZassenAangevraagdError() {
		return (zasStatus.maxAantalAanvragenBereikt) &&
			<FormErrorComponent text={getString(properties.error.zas_te_vaak_aangevraagd)}/>
	}

}

export default ZasAanvragenPage
