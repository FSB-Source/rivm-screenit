package nl.rivm.screenit.main.web.gebruiker.testen.hl7bericht;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum TestHL7BerichtTypeEnum
{
	MAMMA_ORM,

	MAMMA_ORM_VANUIT_IMS,

	MAMMA_ORM_UPLOAD_BEELDEN,

	MAMMA_ORM_UPLOAD_BEELDEN_VANUIT_IMS,

	MAMMA_ILM_UPLOAD_BEELDEN_VANUIT_IMS,

	MAMMA_ILM_VANUIT_IMS,

	MAMMA_ADT,

	DK_IFOBT;
}
