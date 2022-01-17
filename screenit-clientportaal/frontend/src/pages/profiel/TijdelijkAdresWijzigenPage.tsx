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
import React from "react"
import ActieBasePage from "../ActieBasePage"
import {getString} from "../../utils/TekstPropertyUtil"
import {useThunkDispatch} from "../../index"
import {saveTijdelijkAdres} from "../../api/AdresWijzigenThunkAction"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import {navigateAndShowToast} from "../../utils/NavigationUtil"
import properties from "./TijdelijkAdresWijzigenPage.json"
import TijdelijkAdresWijzigenForm from "../../components/form/adres/TijdelijkAdresWijzigenForm"

const TijdelijkAdresWijzigenPage = () => {
    const tijdelijkAdres = useSelector((state: State) => state.client.persoon.tijdelijkAdres)
    const dispatch = useThunkDispatch()

    return (
        <ActieBasePage
            bvoName={""}
            title={tijdelijkAdres ? getString(properties.page.title.wijzigen) : getString(properties.page.title.doorgeven)}
            description={tijdelijkAdres ? getString(properties.page.description.wijzigen) : getString(properties.page.description.doorgeven)}>

            <TijdelijkAdresWijzigenForm huidigTijdelijkAdres={tijdelijkAdres}
                                        onSubmitSucces={(adres) => {
                                            if (tijdelijkAdres) {
                                                adres.id = tijdelijkAdres.id
                                            }
                                            dispatch(saveTijdelijkAdres(adres)).then(() => {
                                                navigateAndShowToast("/profiel", getString(properties.toast.title), getString(properties.toast.description))
                                            })
                                        }}/>
        </ActieBasePage>
    )

}

export default TijdelijkAdresWijzigenPage
