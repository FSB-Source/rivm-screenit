package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.function.UnaryOperator;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.util.functionalinterfaces.BvoHouder;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public final class EnumStringUtil
{

	public static <T extends Enum<T>> String getPropertyString(T value)
	{
		return getPropertyString(value, null);
	}

	public static <T extends Enum<T>> String getPropertyString(T value, Class<T> nullValidClass)
	{
		if (value != null)
		{

			return value.getDeclaringClass().getSimpleName() + '.' + value.name();
		}
		else if (nullValidClass != null)
		{
			return nullValidClass.getSimpleName() + ".null";
		}
		else
		{
			return null;
		}
	}

	public static <T extends Enum<T> & BvoHouder> String maakStringMetBvoEnEnumPropertyString(T enumMetBvo,
		UnaryOperator<String> getString)
	{
		String bvoNamen = Bevolkingsonderzoek.getAfkortingen(enumMetBvo.getBevolkingsonderzoeken());
		return String.format("%s - %s", bvoNamen, getString.apply(EnumStringUtil.getPropertyString(enumMetBvo)));
	}
}
