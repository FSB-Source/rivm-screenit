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
import {State} from "../datatypes/State"
import {Bevolkingsonderzoek, Bevolkingsonderzoeken} from "../datatypes/Bevolkingsonderzoek"

export const selectBehoortTotMammaDoelgroep = (state: State) => state.landingOverzicht.behoortTotMammaDoelgroep
export const selectBehoortTotCervixDoelgroep = (state: State) => state.landingOverzicht.behoortTotCervixDoelgroep
export const selectBehoortTotColonDoelgroep = (state: State) => state.landingOverzicht.behoortTotColonDoelgroep
export const selectBvos = (state: State) => {
	const behoortTotBvos: Array<Bevolkingsonderzoek | null> = Bevolkingsonderzoeken.filter(bvo => {
		return (bvo === Bevolkingsonderzoek.MAMMA && state.landingOverzicht.behoortTotMammaDoelgroep) || (bvo === Bevolkingsonderzoek.COLON && state.landingOverzicht.behoortTotColonDoelgroep) || (bvo === Bevolkingsonderzoek.CERVIX && state.landingOverzicht.behoortTotCervixDoelgroep)
	})
	behoortTotBvos.push(null)
	return behoortTotBvos
}
