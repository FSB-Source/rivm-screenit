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
import {BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import ActieBasePage from "../../ActieBasePage"
import {FormErrorComponent} from "../../../components/form_error/FormErrorComponent"
import {getString} from "../../../utils/TekstPropertyUtil"
import {useThunkDispatch} from "../../../index"
import {getHuidigeCervixUitstel, getHuidigeCervixUitstelStatus, getUitstellenTotDatum, saveCervixUitstel} from "../../../api/CervixUitstelThunkAction"
import properties from "./CervixUitstelAanvragenPage.json"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import {formatDateText} from "../../../utils/DateUtil"
import CervixUitstelAanvragenForm from "./CervixUitstelAanvragenForm"
import {showToast} from "../../../utils/ToastUtil"
import {useNavigate} from "react-router-dom"

const CervixUitstelAanvragenPage = () => {
	const selectedBvo = useSelectedBvo()!
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const cervixUitstel = useSelector((state: State) => state.client.cervixDossier.uitstel)
	const uitstelStatus = useSelector((state: State) => state.client.cervixDossier.uitstelStatus)
	const geboortedatumDisplay = useSelector((state: State) => state.client.persoon.geboortedatumDisplay)

	useEffect(() => {
		dispatch(getHuidigeCervixUitstelStatus())
		dispatch(getHuidigeCervixUitstel())
	}, [dispatch])
	const minPeriodeTussenZwangerEnOnderzoek = uitstelStatus ? uitstelStatus.uitstelBijZwangerschap : 42

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[selectedBvo]}
			title={getString(properties.page.title)}
			description={getString(properties.page.description)}>

			{cervixUitstel.isInSync && getZasAangevraagdError()}

			{cervixUitstel.isInSync && <CervixUitstelAanvragenForm
				cervixUitstel={cervixUitstel} geboortedatumDisplay={geboortedatumDisplay} onSubmitSucces={(initialValues) =>
				dispatch(saveCervixUitstel(initialValues, minPeriodeTussenZwangerEnOnderzoek)).then(() => {
					showToast(getString(properties.toast.title, [formatDateText(getUitstellenTotDatum(
						initialValues, minPeriodeTussenZwangerEnOnderzoek))]), getString(properties.toast.description))
					navigate("/cervix")
				})} uitstelStatus={uitstelStatus}/>}

		</ActieBasePage>
    )

	function getZasAangevraagdError() {
		return (uitstelStatus && uitstelStatus.zasAanvraagInBehandeling) &&
			<FormErrorComponent text={getString(properties.error.zas)}/>
	}
}

export default CervixUitstelAanvragenPage
