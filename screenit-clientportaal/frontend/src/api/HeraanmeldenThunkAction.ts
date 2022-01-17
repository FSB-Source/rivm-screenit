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
import ScreenitBackend from "../utils/Backend"
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"
import {ResetBeschikbareContactActies, setBeschikbareContactActies} from "../actions/ContactActiesReduxAction"
import {Dispatch} from "redux"
import {ContactActiesDto} from "../datatypes/ContactActiesDto"

export const saveHeraanmeldVerzoekEnGeefBeschikbareActies = (bvo: Bevolkingsonderzoek, wilNieuweUitnodigingOntvangen: boolean, dispatch: Dispatch<ResetBeschikbareContactActies>) => (): Promise<ContactActiesDto> => {
	return new Promise<ContactActiesDto>((resolve => {
		ScreenitBackend.post(`/heraanmelden/${bvo}/${wilNieuweUitnodigingOntvangen}`).then(() => {
			ScreenitBackend.get(`/acties/beschikbaar`)
				.then(async response => {
					await dispatch(setBeschikbareContactActies(response.data))
					resolve(response.data)
				})
		})
	}))
}
