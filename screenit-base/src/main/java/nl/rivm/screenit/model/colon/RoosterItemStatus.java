
package nl.rivm.screenit.model.colon;

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

import nl.rivm.screenit.model.colon.planning.EventType;

public enum RoosterItemStatus
{

	BLOKKADE(EventType.PURPLE),

	GEBRUIKT_VOOR_CAPACITEIT(EventType.ORANGE),

	INTAKE_GEPLAND(EventType.RED),

	VRIJ_TE_VERPLAATSEN(EventType.DARKGREEN);

	private final EventType eventType;

	RoosterItemStatus(EventType eventType)
	{
		this.eventType = eventType;
	}

	public EventType getEventType()
	{
		return eventType;
	}
}
