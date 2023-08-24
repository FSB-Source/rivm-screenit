package nl.rivm.screenit.main.model.mamma.beoordeling;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class BeoordelingenReserveringResult
{
	private final Long eersteGereserveerdeBeoordelingId;

	private final List<Long> volgendeGereserveerdeBeoordelingenIds;

	private String infoMessage;

	public BeoordelingenReserveringResult()
	{
		this(Collections.emptyList());
	}

	public BeoordelingenReserveringResult(List<Long> gereserveerdeBeoordelingenIds)
	{
		eersteGereserveerdeBeoordelingId = gereserveerdeBeoordelingenIds.isEmpty() ? null : gereserveerdeBeoordelingenIds.get(0);
		volgendeGereserveerdeBeoordelingenIds = new ArrayList<>(gereserveerdeBeoordelingenIds);
		volgendeGereserveerdeBeoordelingenIds.remove(eersteGereserveerdeBeoordelingId);
	}

	public Long getEersteGereserveerdeBeoordelingId()
	{
		return eersteGereserveerdeBeoordelingId;
	}

	public List<Long> getVolgendeGereserveerdeBeoordelingenIds()
	{
		return volgendeGereserveerdeBeoordelingenIds;
	}

	public String getInfoMessage()
	{
		return infoMessage;
	}

	public void setInfoMessage(String infoMessage)
	{
		this.infoMessage = infoMessage;
	}
}
