/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import {Bevolkingsonderzoek} from "./Bevolkingsonderzoek"

export enum BezwaarOverigeType {
    GEEN_OPNAME_VANUIT_BRP = "GEEN_OPNAME_VANUIT_BRP",
    VERZOEK_VERNIETIGING_DOSSIER = "VERZOEK_VERNIETIGING_DOSSIER",
    GEEN_UITWISSELING_PALGA = "GEEN_UITWISSELING_PALGA",
    GEEN_REGISTRATIE_GEBOORTELAND = "GEEN_REGISTRATIE_GEBOORTELAND",
    GEEN_UITWISSELING_SO_CC = "GEEN_UITWISSELING_SO_CC"
}

export const getOverigeBezwaren: { [bvo in Bevolkingsonderzoek]: BezwaarOverigeType[] } = {
    [Bevolkingsonderzoek.MAMMA]: [BezwaarOverigeType.GEEN_OPNAME_VANUIT_BRP, BezwaarOverigeType.VERZOEK_VERNIETIGING_DOSSIER, BezwaarOverigeType.GEEN_REGISTRATIE_GEBOORTELAND],
    [Bevolkingsonderzoek.CERVIX]: [BezwaarOverigeType.GEEN_OPNAME_VANUIT_BRP, BezwaarOverigeType.VERZOEK_VERNIETIGING_DOSSIER, BezwaarOverigeType.GEEN_UITWISSELING_PALGA, BezwaarOverigeType.GEEN_REGISTRATIE_GEBOORTELAND],
    [Bevolkingsonderzoek.COLON]: [BezwaarOverigeType.GEEN_OPNAME_VANUIT_BRP, BezwaarOverigeType.VERZOEK_VERNIETIGING_DOSSIER, BezwaarOverigeType.GEEN_REGISTRATIE_GEBOORTELAND, BezwaarOverigeType.GEEN_UITWISSELING_SO_CC],
}
