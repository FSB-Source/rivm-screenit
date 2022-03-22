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
import {Dispatch} from "redux"
import ScreenitBackend from "../utils/Backend"
import {ColonIntakeAfspraakAction, setColonIntakeAfspraakAction} from "../actions/ColonDossierAction"
import RedenAfspraakAfzeggen from "../datatypes/colon/RedenAfspraakAfzeggen"

export const getHuidigeIntakeAfspraak = () => async (dispatch: Dispatch<ColonIntakeAfspraakAction>) => {
    return ScreenitBackend.get("/colon/afspraak/huidig")
        .then(response => dispatch(setColonIntakeAfspraakAction(response.data)))
}

export const saveColonAfzegReden = (afzegReden: RedenAfspraakAfzeggen) => () => {
    return ScreenitBackend.put(`/colon/afspraak/afzeggen/${afzegReden}`)
}