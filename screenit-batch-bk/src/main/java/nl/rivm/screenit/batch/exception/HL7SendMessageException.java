package nl.rivm.screenit.batch.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

public class HL7SendMessageException extends Exception
{
	private final String hl7Message;

	public HL7SendMessageException(String exceptionMessage, String hl7Message, Throwable cause)
	{
		super(exceptionMessage, cause);
		this.hl7Message = hl7Message;
	}

	public HL7SendMessageException(String exceptionMessage, String hl7Message)
	{
		super(exceptionMessage);
		this.hl7Message = hl7Message;
	}

	public String getHl7Message()
	{
		return hl7Message;
	}

}
