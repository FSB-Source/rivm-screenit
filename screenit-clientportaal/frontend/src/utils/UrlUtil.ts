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
import {Bevolkingsonderzoek} from "../datatypes/Bevolkingsonderzoek"
import {RoutePath} from "../routes/routes"

export function getBvoBaseUrl(bvo: Bevolkingsonderzoek | undefined): RoutePath {
	return bvo ? `/${bvo.toLowerCase()}` as RoutePath : "/"
}

export function getAfmeldenUrl(bvo: Bevolkingsonderzoek): RoutePath {
	return `/${bvo.toLowerCase()}/afmelden` as RoutePath
}

export function getBezwaarUrl(bvo: Bevolkingsonderzoek): RoutePath {
	return `/${bvo.toLowerCase()}/bezwaar` as RoutePath
}

export function getContactUrl(regio: string) {
	return regio ? `${getBevolkingsonderzoekNederlandUrl()}/over-ons/${regio.replace(" ", "-").toLowerCase()}` : `${getBevolkingsonderzoekNederlandUrl()}/over-ons/`
}

export function getBevolkingsonderzoekNederlandUrl() {
	return "https:
}

export function getBevolkingsonderzoekNederlandUrlNaam() {
	return "bevolkingsonderzoeknederland.nl"
}

export function isExternalUrl(link: String) {
	const str = "https:
	return link.match(str)

}
