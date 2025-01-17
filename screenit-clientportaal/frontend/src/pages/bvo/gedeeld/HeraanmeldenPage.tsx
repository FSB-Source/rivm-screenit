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
import {useThunkDispatch} from "../../../index"
import {useSelectedBvo} from "../../../utils/Hooks"
import React, {useEffect, useState} from "react"
import ActieBasePage from "../../ActieBasePage"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import bvoStyle from "../../../components/BvoStyle.module.scss"
import {saveHeraanmeldVerzoekEnGeefBeschikbareActies} from "../../../api/HeraanmeldenThunkAction"
import {ArrowType} from "../../../components/vectors/ArrowIconComponent"
import {getString} from "../../../utils/TekstPropertyUtil"
import ScreenitBackend from "../../../utils/Backend"
import {geenHeraanmeldenOpties, HeraanmeldenOptiesDto} from "../../../datatypes/afmelden/HeraanmeldenOptiesDto"
import HeraanmeldenAfspraakMakenForm from "../../../components/form/heraanmelden/HeraanmeldenAfspraakMakenForm"
import HeraanmeldenUitnodigingAanvragenForm from "../../../components/form/heraanmelden/HeraanmeldenUitnodigingAanvragenForm"
import {getBvoBaseUrl} from "../../../utils/UrlUtil"
import LadenComponent from "../../../components/laden/LadenComponent"
import SubmitButton from "../../../components/input/SubmitButton"
import {ClientContactActieType} from "../../../datatypes/ClientContactActieType"
import {useNavigate} from "react-router-dom"
import {showToast} from "../../../utils/ToastUtil"

const HeraanmeldenPage = () => {
	const properties = require("./HeraanmeldenPage.json")
	const dispatch = useThunkDispatch()
	const selectedBvo = useSelectedBvo()!
	const navigate = useNavigate()
	const [heraanmeldenOpties, setHeraanmeldenOpties] = useState<HeraanmeldenOptiesDto>(geenHeraanmeldenOpties)
	const [optiesZijnOpgehaald, setOptiesZijnOpgehaald] = useState<boolean>(false)

	useEffect(() => {
		ScreenitBackend.get(`/heraanmelden/${selectedBvo}`)
			.then((response) => {
				setHeraanmeldenOpties(response.data)
				setOptiesZijnOpgehaald(true)
			})
	}, [selectedBvo])

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[selectedBvo]}
			title={getString(properties.title.heraanmelden)}
			description={getDescription()}>

			{!optiesZijnOpgehaald && <LadenComponent/>}

			{optiesZijnOpgehaald && heraanmeldenOpties.magColonUitnodigingAanvragen &&
				<HeraanmeldenUitnodigingAanvragenForm onSubmitSucces={(uitnodigingAanvragen) => {
					dispatch(saveHeraanmeldVerzoekEnGeefBeschikbareActies(selectedBvo, uitnodigingAanvragen, dispatch))
						.then(() => {
							showToast(undefined, uitnodigingAanvragen ? getString(properties.toast.COLON.uitnodiging) : getString(properties.toast.COLON.algemeen))
							navigate(getBvoBaseUrl(selectedBvo))
						})
				}}/>}

			{optiesZijnOpgehaald && heraanmeldenOpties.magColonIntakAfspraakInplannen &&
				<HeraanmeldenAfspraakMakenForm onSubmitSucces={(afspraakMaken) => {
					dispatch(saveHeraanmeldVerzoekEnGeefBeschikbareActies(selectedBvo, false, dispatch))
						.then(() => {
							showToast(undefined, afspraakMaken ? getString(properties["toast"][selectedBvo]["afspraak"]) : getString(properties["toast"][selectedBvo]["algemeen"]))
							navigate(afspraakMaken ? "/colon/afspraak-maken-heraanmelding/" : getBvoBaseUrl(selectedBvo))
						})
				}}/>}

			{optiesZijnOpgehaald && !heraanmeldenOpties.magColonIntakAfspraakInplannen && !heraanmeldenOpties.magColonUitnodigingAanvragen &&
				<SubmitButton className={bvoStyle.baseBackgroundColor} label={getString(properties.form.button)} displayArrow={ArrowType.ARROW_RIGHT} onClick={() => {
					dispatch(saveHeraanmeldVerzoekEnGeefBeschikbareActies(selectedBvo, false, dispatch)).then((beschikbareActies) => {
						const kanDirectMammaAfspraakMaken = selectedBvo === Bevolkingsonderzoek.MAMMA && beschikbareActies.beschikbareActies.includes(ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
						showToast(undefined, kanDirectMammaAfspraakMaken ? getString(properties.toast.MAMMA.afspraak) : getString(properties["toast"][selectedBvo]["algemeen"]))
						navigate(kanDirectMammaAfspraakMaken ? "/mamma/afspraak" : getBvoBaseUrl(selectedBvo))
					})
				}}/>}

		</ActieBasePage>
	)

	function getDescription(): string {
		if (heraanmeldenOpties.magColonUitnodigingAanvragen) {
			return getString(properties.afmeldenText) + "<br><br>" + getString(properties.COLON.uitnodiging.infoText) + getString(properties.COLON.uitnodiging.hintText)
		}
		if (heraanmeldenOpties.magColonIntakAfspraakInplannen) {
			return getString(properties.afmeldenText) + "<br><br>" + getString(properties.COLON.afspraak.infoText) + "<br><br>" + getString(properties.COLON.afspraak.hintText)
		}
		if (selectedBvo === Bevolkingsonderzoek.MAMMA) {
			return getString(properties.afmeldenText)
		} else {
			return getString(properties.afmeldenText) + "<br><br>" + getString(selectedBvo === Bevolkingsonderzoek.CERVIX ? properties.uitnodigingText_bmhk : properties.uitnodigingText_dk_bk)
		}
	}
}

export default HeraanmeldenPage
