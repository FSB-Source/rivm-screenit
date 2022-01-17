package nl.rivm.screenit.model.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.topicuszorg.planning.model.IEventType;

public enum EventType implements IEventType
{

	RED("rood"),

	GREEN("groen"),

	DARKGREEN("donkergroen"),

	YELLOW("geel"),

	BLUE("blauw"),

	PURPLE("paars"),

	LIGHT_PURPLE("licht-paars"),

	BLACK("zwart"),

	GREY("grijs"),

	ORANGE("oranje"),

	LIGHT_BLUE("licht-blauw"),

	LIGHT_GREEN("licht-groen"),

	BROWN("bruin"),

	PINK("roze"),

	LIME("lime"),

	TURQUOISE("turquoise");

	private String name;

	private EventType(String name)
	{
		this.name = name;
	}

	@Override
	public String getName()
	{
		return name;
	}

	@Override
	public String toString()
	{
		return name;
	}
}
