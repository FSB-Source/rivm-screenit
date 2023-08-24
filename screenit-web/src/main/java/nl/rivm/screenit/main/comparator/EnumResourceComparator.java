package nl.rivm.screenit.main.comparator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Comparator;

import org.apache.wicket.Component;
import org.apache.wicket.util.lang.Classes;

public class EnumResourceComparator<T extends Enum> implements Comparator<T>
{
	private Component component;

	public EnumResourceComparator(Component c)
	{
		this.setComponent(c);
	}

	protected String resourceKey(T value)
	{
		return Classes.simpleName(value.getDeclaringClass()) + '.' + value.name();
	}

	@Override
	public int compare(T o1, T o2)
	{
		if (component != null)
		{
			String name1 = component.getString(resourceKey(o1));
			String name2 = component.getString(resourceKey(o2));
			return name1.compareTo(name2);
		}
		return 0;
	}

	public Component getComponent()
	{
		return component;
	}

	public void setComponent(Component component)
	{
		this.component = component;
	}

}
