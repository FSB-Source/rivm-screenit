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
import {ContactActiesDto, geenBeschikbareContactActies} from "../datatypes/ContactActiesDto";
import {ContactActiesAction, RESET_BESCHIKBARE_CONTACT_ACTIES} from "../actions/ContactActiesReduxAction";

function ContactActiesReducer(stateSlice: ContactActiesDto = geenBeschikbareContactActies, action: ContactActiesAction): ContactActiesDto {
    switch (action.type) {
        case RESET_BESCHIKBARE_CONTACT_ACTIES:
            return action.beschikbareContactActies
        default:
            return stateSlice;
    }
}

export default ContactActiesReducer
