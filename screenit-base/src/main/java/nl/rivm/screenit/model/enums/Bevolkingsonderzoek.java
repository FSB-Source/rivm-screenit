package nl.rivm.screenit.model.enums;

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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

public enum Bevolkingsonderzoek
{
	COLON("Darmkanker", "DK"),

	CERVIX("Baarmoederhalskanker", "BMHK"),

	MAMMA("Borstkanker", "BK")

	;

	private final String naam;

	private final String afkorting;

	private Bevolkingsonderzoek(String naam, String afkorting)
	{
		this.naam = naam;
		this.afkorting = afkorting;
	}

	public String getAfkorting()
	{
		return afkorting;
	}

	public String getNaam()
	{
		return naam;
	}

	public static String getAfkortingen(Bevolkingsonderzoek... onderzoeken)
	{
		return getAfkortingen(new ArrayList<Bevolkingsonderzoek>(Arrays.asList(onderzoeken)));
	}

	public static String getAfkortingen(List<Bevolkingsonderzoek> onderzoeken)
	{
		sort(onderzoeken);
		String bvoString = "";
		List<String> bvoAfkortingen = onderzoeken
			.stream()
			.map(Bevolkingsonderzoek::getAfkorting)
			.collect(Collectors.toList());
		bvoString = StringUtils.join(bvoAfkortingen, ", ");
		return bvoString;
	}

	public static void sort(List<Bevolkingsonderzoek> onderzoeken)
	{
		Collections.sort(onderzoeken, new Comparator<Bevolkingsonderzoek>()
		{
			@Override
			public int compare(Bevolkingsonderzoek o1, Bevolkingsonderzoek o2)
			{
				return o1.getNaam().compareTo(o2.getNaam());
			}

		});
	}

	public static boolean heeftAlleBevolkingsonderzoeken(List<Bevolkingsonderzoek> onderzoeken)
	{
		return onderzoeken.containsAll(Arrays.asList(values()));
	}

	public static boolean alleenDarmkanker(List<Bevolkingsonderzoek> onderzoeken)
	{
		return (onderzoeken.size() == 1 && onderzoeken.contains(COLON));
	}

	public static boolean alleenBaarmoederhalskanker(List<Bevolkingsonderzoek> onderzoeken)
	{
		return (onderzoeken.size() == 1 && onderzoeken.contains(CERVIX));
	}

	public static boolean alleenBorstkanker(List<Bevolkingsonderzoek> onderzoeken)
	{
		return (onderzoeken.size() == 1 && onderzoeken.contains(MAMMA));
	}

}
