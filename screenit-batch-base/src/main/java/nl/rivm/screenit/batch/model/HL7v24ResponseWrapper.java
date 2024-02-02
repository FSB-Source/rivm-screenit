package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.berichten.ScreenITResponseV24MessageWrapper;

import org.apache.commons.lang.StringUtils;

@Getter
@Setter
public class HL7v24ResponseWrapper
{
	private ScreenITResponseV24MessageWrapper responseV24MessageWrapper;

	private String melding;

	private Exception crashException;

	private final List<String> succesAckCodes = List.of("AA", "CA");

	private final List<String> errorAckCodes = List.of("AE", "CE");

	public boolean isError()
	{
		if (responseV24MessageWrapper != null)
		{
			return errorAckCodes.contains(responseV24MessageWrapper.getAcknowledgmentCodeString());
		}
		else
		{
			return false;
		}
	}

	public boolean isSuccess()
	{
		if (responseV24MessageWrapper != null)
		{
			return succesAckCodes.contains(responseV24MessageWrapper.getAcknowledgmentCodeString());
		}
		return StringUtils.isBlank(melding);
	}

}
