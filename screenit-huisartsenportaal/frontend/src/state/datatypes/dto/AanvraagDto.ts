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
import {AbstractDtoReferenceObject} from "./AbstractDtoReferenceObject"
import {LocatieDto} from "./LocatieDto"

export interface AanvraagDto extends AbstractDtoReferenceObject {
	aanvraagDatum?: string;
	aantal?: number;
	status?: AanvraagStatus;
	statusDatum?: string;
	locatie: LocatieDto;
	aangevraagdDoor?: string;
}

export interface NieuweAanvraagDto {
	aantal: number;
	locatie: LocatieDto | null;
}

export enum AanvraagStatus {
	AANGEVRAAGD = "AANGEVRAAGD",
	AFGEDRUKT_KLAAR_OM_TE_VERSTUREN = "AFGEDRUKT_KLAAR_OM_TE_VERSTUREN",
	AFGEDRUKT_EN_VERSTUURD = "AFGEDRUKT_EN_VERSTUURD",
	VERWIJDERD = "VERWIJDERD"
}

export const getAanvraagStatusText: { [key in AanvraagStatus]: string } = {
	[AanvraagStatus.AANGEVRAAGD]: "Aangevraagd",
	[AanvraagStatus.AFGEDRUKT_KLAAR_OM_TE_VERSTUREN]: "Afgedrukt en klaar om te versturen",
	[AanvraagStatus.AFGEDRUKT_EN_VERSTUURD]: "Afgedrukt en verstuurd",
	[AanvraagStatus.VERWIJDERD]: "Verwijderd",
}
