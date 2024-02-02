/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import * as Yup from "yup"
import {getString} from "./TekstPropertyUtil"
import properties from "./ValidatieUtil.json"
import validatieProperties from "./ValidatieUtil.json"

export function wachtwoordValidatie(veldNaam: string) {
	return Yup.string()
		.nullable()
		.min(12, getString(properties.minLength, [veldNaam]))
		.max(255, getString(properties.maxLength, [veldNaam]))
		.matches(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[^A-Za-z0-9])/, getString(properties.wachtwoord))
}

export function postcodeValidatie() {
	return Yup.string()
		.matches(/^[0-9]{4}[a-zA-Z]{2}/, validatieProperties.postcode)
}

export function zorgmailKlantnummerValidatie(veldNaam: string) {
	return Yup.string()
		.matches(/^[0-9]*$/, getString(properties.zorgmailklantnummer))
		.min(9, getString(properties.minLength, [veldNaam]))
		.max(9, getString(properties.maxLength, [veldNaam]))
}

export function ibanValidatie() {
	return Yup.string()
		.matches(/^([A-Z]{2}[ -]?[0-9]{2})(?=(?:[ -]?[A-Z0-9]){9,30}$)((?:[ -]?[A-Z0-9]{3,5}){2,7})([ -]?[A-Z0-9]{1,3})?$/, getString(properties.iban))
}
