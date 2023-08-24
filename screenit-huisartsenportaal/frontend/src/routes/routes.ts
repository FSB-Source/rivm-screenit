/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import {AppRoute} from "./index"
import {LoginPage} from "../pages/authenticatie/login/LoginPage"
import LabformulierenAanvragenPage from "../pages/labformulieren/LabformulierenAanvragenPage"
import VerrichtingenOverzichtPage from "../pages/verrichtingen/VerrichtingenOverzichtPage"
import BetalingenOverzichtPage from "../pages/betalingen/BetalingenOverzichtPage"
import GegevensWijzigenPage from "../pages/gegevens/GegevensWijzigenPage"
import RegistrerenPage from "../pages/authenticatie/registreren/RegistrerenPage"
import RegistrerenVoltooienPage from "../pages/authenticatie/registreren/voltooien/RegistrerenVoltooienPage"
import WachtwoordResetPage from "../pages/authenticatie/reset/WachtwoordResetPage"
import WachtwoordResetCodePage from "../pages/authenticatie/reset/code/WachtwoordResetCodePage"
import WachtwoordResetVoltooienPage from "../pages/authenticatie/reset/voltooien/WachtwoordResetVoltooienPage"
import {AuthenticationScope} from "../state/datatypes/enums/AuthenticationScope"
import {Recht} from "../state/datatypes/enums/Recht"

export const routes: AppRoute[] = [
	{
		path: "/login",
		component: LoginPage,
	},
	{
		path: "/registreren/voltooien",
		scope: AuthenticationScope.REGISTREREN,
		component: RegistrerenVoltooienPage,
	},
	{
		path: "/registreren",
		component: RegistrerenPage,
	},
	{
		path: "/overeenkomst",
		scope: AuthenticationScope.LOGIN,
		recht: Recht.ROLE_OVEREENKOMST,
		component: GegevensWijzigenPage,
	},
	{
		path: "/wachtwoordvergeten",
		component: WachtwoordResetPage,
	},
	{
		path: "/wachtwoordvergeten/registreren",
		component: WachtwoordResetCodePage,
	},
	{
		path: "/wachtwoordvergeten/voltooien",
		scope: AuthenticationScope.WACHTWOORDVERGETEN,
		component: WachtwoordResetVoltooienPage,
	},
	{
		path: "/",
		scope: AuthenticationScope.LOGIN,
		recht: Recht.ROLE_AANVRAGEN,
		component: LabformulierenAanvragenPage,
	},
	{
		path: "/verrichtingen",
		scope: AuthenticationScope.LOGIN,
		recht: Recht.ROLE_AANVRAGEN,
		component: VerrichtingenOverzichtPage,
	},
	{
		path: "/betalingen",
		scope: AuthenticationScope.LOGIN,
		recht: Recht.ROLE_AANVRAGEN,
		component: BetalingenOverzichtPage,
	},
	{
		path: "/gegevens",
		scope: AuthenticationScope.LOGIN,
		recht: Recht.ROLE_AANVRAGEN,
		component: GegevensWijzigenPage,
	},
]
