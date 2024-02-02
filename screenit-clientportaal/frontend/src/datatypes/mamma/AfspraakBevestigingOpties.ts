/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import {KandidaatAfspraak} from "./KandidaatAfspraak"
import {BevestigingsType} from "../BevestigingsType"

export class AfspraakBevestigingOpties {
	afspraakId: string
	toonBriefOptie: boolean
	toonSmsOptie: boolean
	bevestigingsType: BevestigingsType = BevestigingsType.GEEN
	wilHerinneringsSms: boolean = false
	clientNieuwEmailAdres: string = ""
	clientNieuwMobielNummer: string = ""

	constructor(id: string, gemaakteAfspraak: KandidaatAfspraak) {
		this.afspraakId = id
		this.toonBriefOptie = gemaakteAfspraak.toonBevestigingsBriefOptie
		this.toonSmsOptie = gemaakteAfspraak.toonSmsHerinneringOptie
		if (gemaakteAfspraak.clientEmailAdres != null) {
			this.clientNieuwEmailAdres = gemaakteAfspraak.clientEmailAdres
		}
		if (gemaakteAfspraak.clientMobielNummer != null) {
			this.clientNieuwMobielNummer = gemaakteAfspraak.clientMobielNummer
		}
	}

	resetKeuzes() {
		this.bevestigingsType = BevestigingsType.GEEN
		this.wilHerinneringsSms = false
	}

}
