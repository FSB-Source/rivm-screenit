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
import bvoStyle from "../components/BvoStyle.module.scss"

export enum Bevolkingsonderzoek {
    MAMMA = "MAMMA",
    CERVIX = "CERVIX",
    COLON = "COLON"
}

export const Bevolkingsonderzoeken = [Bevolkingsonderzoek.MAMMA, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON]

export const BevolkingsonderzoekNaam: { [key in Bevolkingsonderzoek]: string } = {
	[Bevolkingsonderzoek.MAMMA]: "borstkanker",
	[Bevolkingsonderzoek.CERVIX]: "baarmoederhalskanker",
	[Bevolkingsonderzoek.COLON]: "darmkanker",
}

export const BevolkingsonderzoekStyle: { [key in Bevolkingsonderzoek]: string } = {
    [Bevolkingsonderzoek.MAMMA]: bvoStyle.mamma,
    [Bevolkingsonderzoek.CERVIX]: bvoStyle.cervix,
    [Bevolkingsonderzoek.COLON]: bvoStyle.colon,
}

export const BevolkingsonderzoekToptaakStyle: { [key in Bevolkingsonderzoek]: string } = {
    [Bevolkingsonderzoek.MAMMA]: bvoStyle.mammaIcon,
    [Bevolkingsonderzoek.CERVIX]: bvoStyle.cervixIcon,
    [Bevolkingsonderzoek.COLON]: bvoStyle.colonIcon,
}
