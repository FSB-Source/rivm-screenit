/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import BasePage from "../../../BasePage"
import properties from "./RegistrerenVoltooienPage.json"
import {getString} from "../../../../util/TekstPropertyUtil"
import GegevensWijzigenFormComponent from "../../../../components/gegevens/GegevensWijzigenFormComponent"

const RegistrerenVoltooienPage = () => {
	return <BasePage title={getString(properties.title)} description={getString(properties.description)}>
		<GegevensWijzigenFormComponent/>
	</BasePage>
}

export default RegistrerenVoltooienPage
