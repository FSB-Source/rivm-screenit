package nl.rivm.screenit.model.mamma.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.List;

public enum MammaMammografieIlmStatus
{

	NIET_BESCHIKBAAR,

	BESCHIKBAAR,

	TE_VERWIJDEREN,

	VERWIJDEREN_MISLUKT,

	VERWIJDERD;

	public static final List<MammaMammografieIlmStatus> BEELDEN_BESCHIKBAAR_OF_BESCHIKBAAR_GEWEEST = Arrays.asList(BESCHIKBAAR, TE_VERWIJDEREN, VERWIJDEREN_MISLUKT, VERWIJDERD);

	public static boolean beeldenBeschikbaar(MammaMammografieIlmStatus status)
	{
		return BESCHIKBAAR.equals(status);
	}

	public static boolean beeldenBeschikbaarOfBeschikbaarGeweest(MammaMammografieIlmStatus status)
	{
		return BEELDEN_BESCHIKBAAR_OF_BESCHIKBAAR_GEWEEST.contains(status);
	}

	public static boolean beeldenMogelijkAanwezig(MammaMammografieIlmStatus status)
	{
		return Arrays.asList(BESCHIKBAAR, TE_VERWIJDEREN, VERWIJDEREN_MISLUKT).contains(status);
	}
}
