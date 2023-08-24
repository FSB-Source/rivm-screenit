package nl.rivm.screenit.model.enums;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public enum Bevolkingsonderzoek
{
	COLON("Darmkanker", "DK"),

	CERVIX("Baarmoederhalskanker", "BMHK"),

	MAMMA("Borstkanker", "BK");

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
		return getAfkortingen(new ArrayList<>(Arrays.asList(onderzoeken)));
	}

	public static String getAfkortingen(List<Bevolkingsonderzoek> onderzoeken)
	{
		return sort(onderzoeken).stream().map(Bevolkingsonderzoek::getAfkorting).collect(Collectors.joining(", "));
	}

	public static List<Bevolkingsonderzoek> sort(List<Bevolkingsonderzoek> onderzoeken)
	{
		return onderzoeken.stream().sorted(Comparator.comparing(Bevolkingsonderzoek::getNaam)).collect(Collectors.toList());
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
