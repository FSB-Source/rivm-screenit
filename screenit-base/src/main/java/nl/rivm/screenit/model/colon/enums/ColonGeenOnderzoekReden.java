
package nl.rivm.screenit.model.colon.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public enum ColonGeenOnderzoekReden
{
	VERZOEK_CLIENT(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_VERZOEK_CLIENT),

	MEDISCHE_REDENEN(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_MEDISCHE_REDENEN),

	TERUG_NAAR_SCREENING_2_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_2_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_3_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_3_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_4_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_4_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_5_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_5_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_6_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_6_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_7_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_7_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_8_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_8_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_9_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_9_JAAR_TERUG_NAAR_SCREENING),

	TERUG_NAAR_SCREENING_10_JAAR(ColonUitnodigingsintervalType.INTAKE_GEEN_VERVOLGBELEID_10_JAAR_TERUG_NAAR_SCREENING),

	;

	private final ColonUitnodigingsintervalType uitnodigingsintervalType;

	ColonGeenOnderzoekReden(ColonUitnodigingsintervalType uitnodigingsintervalType)
	{
		this.uitnodigingsintervalType = uitnodigingsintervalType;

	}

	public ColonUitnodigingsintervalType getUitnodigingsintervalType()
	{
		return uitnodigingsintervalType;
	}

	public static ColonGeenOnderzoekReden getTerugNaarScreeningReden(int aantal)
	{
		return ColonGeenOnderzoekReden.valueOf("TERUG_NAAR_SCREENING_" + aantal + "_JAAR");
	}

}
