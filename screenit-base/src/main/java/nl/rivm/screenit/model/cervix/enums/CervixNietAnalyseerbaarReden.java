package nl.rivm.screenit.model.cervix.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.INaam;

public enum CervixNietAnalyseerbaarReden implements INaam
{
	
	ONBEKEND(new ArrayList<>(), "Onbekend"),

	HOUDBAARHEID_VERLOPEN_OF_ONBEKEND(new ArrayList<>(), "Houdbaarheid verlopen of onbekend"),

	FYSIEKE_SCHADE(Arrays.asList(CervixMonsterType.UITSTRIJKJE, CervixMonsterType.ZAS), "Fysieke schade"),

	VERONTREINIGING_BUITENKANT(Arrays.asList(CervixMonsterType.UITSTRIJKJE, CervixMonsterType.ZAS), "Verontreiniging buitenkant"),

	ONVOLDOENDE_VOLUME(Arrays.asList(CervixMonsterType.UITSTRIJKJE), "Onvoldoende volume"),

	CERVIX_BRUSH_IN_MONSTER(Arrays.asList(CervixMonsterType.UITSTRIJKJE), "Cervix-Brush in monster"),

	ONJUIST_MEDIUM(Arrays.asList(CervixMonsterType.UITSTRIJKJE), "Onjuist medium"),

	ZAS_AANGELEVERD_ZONDER_DOP(Arrays.asList(CervixMonsterType.ZAS), "ZAS aangeleverd zonder dop"),

	ZAS_GEEN_BORSTEL_AANWEZIG(Arrays.asList(CervixMonsterType.ZAS), "Geen borstel aanwezig");

	private List<CervixMonsterType> monsterTypen;

	private String naam;

	CervixNietAnalyseerbaarReden(List<CervixMonsterType> monsterTypen, String naam)
	{
		this.monsterTypen = monsterTypen;
		this.naam = naam;
	}

	public List<CervixMonsterType> getMonsterTypen()
	{
		return monsterTypen;
	}

	public static List<CervixNietAnalyseerbaarReden> getMogelijkeRedenen(CervixMonsterType monsterType)
	{
		List<CervixNietAnalyseerbaarReden> redenen = new ArrayList<>();
		for (CervixNietAnalyseerbaarReden reden : values())
		{
			if (reden.getMonsterTypen().contains(monsterType))
			{
				redenen.add(reden);
			}
		}
		return redenen;
	}

	public String getNaam()
	{
		return naam;
	}
}
