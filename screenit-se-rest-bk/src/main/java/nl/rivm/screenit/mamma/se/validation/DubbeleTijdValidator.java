package nl.rivm.screenit.mamma.se.validation;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdRedenDto;
import nl.rivm.screenit.mamma.se.dto.actions.SEActieType;

import com.fasterxml.jackson.databind.ObjectMapper;

public class DubbeleTijdValidator extends Validator
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	protected List<SEActieType> actieTypes()
	{
		return Arrays.asList(SEActieType.MAAK_DUBBELE_TIJD, SEActieType.MAAK_DUBBELE_TIJD_REDEN);
	}

	@Override
	protected void validateActions(List<ActionDto> actions)
	{
		if (actions.isEmpty()) {
			return; 
		} else if (actions.size() != 2) {
			StringBuilder sb = new StringBuilder();
			actions.forEach(a -> sb.append(" - " + a.getType()));
			throw new IllegalArgumentException("Er moet (na ontdubbelen) 1 MAAK_DUBBELE_TIJD en 1 MAAK_DUBBELE_TIJD_REDEN actie zijn of geen van beide. Maar nu is er alleen " + sb.toString());
		}
		try
		{
			final MaakDubbeleTijdDto maakDubbeleTijdDto = objectMapper.readValue(actions.get(0).getNodeText(), MaakDubbeleTijdDto.class);
			final MaakDubbeleTijdRedenDto maakDubbeleTijdRedenDto = objectMapper.readValue(actions.get(1).getNodeText(), MaakDubbeleTijdRedenDto.class);
			if (maakDubbeleTijdDto.isDubbeleTijd())
			{
				if (maakDubbeleTijdRedenDto.getDubbeleTijdReden() == null || maakDubbeleTijdRedenDto.getDubbeleTijdReden().isEmpty())
				{
					throw new IllegalArgumentException("Geef een reden voor het aanvinken van dubbele tijd voor deze client.");
				}
			}
			else
			{
				if (maakDubbeleTijdRedenDto.getDubbeleTijdReden() != null)
				{
					throw new IllegalArgumentException("Omdat van deze client dubbele tijd niet is aangevinkt, moet er ook geen dubbele tijd reden zijn.");
				}
			}
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
