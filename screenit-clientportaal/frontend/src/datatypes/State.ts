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
import {leegMammaDossier} from "./MammaDossier"
import {Client} from "./Client"
import {LandingOverzicht, legeLandingOverzicht} from "./landing/LandingOverzicht"
import {leegColonDossier} from "./ColonDossier"
import {ToastMessage} from "./toast/ToastMessage"
import {legePersoon} from "./Persoon"
import {AuthenticatieState, legeAuthenticatieState} from "./authenticatie/AuthenticatieState"
import {geenBeschikbareContactActies} from "./ContactActiesDto"
import {leegCervixDossier} from "./CervixDossier"
import {EnvironmentInfo, geenEnvironmentInfo} from "./EnvironmentInfo"

export type State = {
	authenticatie: AuthenticatieState,
	environmentInfo: EnvironmentInfo,
	client: Client,
	landingOverzicht: LandingOverzicht,
	toasts: ToastMessage[],
}

export const legeState: State = {
	authenticatie: legeAuthenticatieState,
	environmentInfo: geenEnvironmentInfo,
	client: {
		beschikbareActies: geenBeschikbareContactActies,
		cervixDossier: leegCervixDossier,
		colonDossier: leegColonDossier,
		laatsteBezwaarMoment: [],
		mammaDossier: leegMammaDossier,
		persoon: legePersoon,
		regio: "",
	},
	landingOverzicht: legeLandingOverzicht,
	toasts: [],
}
