package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.model.berichten.ScreenITResponseV24MessageWrapper;

public class HL7v24ResponseWrapper
{
	private ScreenITResponseV24MessageWrapper responseV24MessageWrapper;

	private String melding;

	private Exception crashException;

	private boolean success;

	public ScreenITResponseV24MessageWrapper getResponseV24MessageWrapper()
	{
		return responseV24MessageWrapper;
	}

	public void setResponseV24MessageWrapper(ScreenITResponseV24MessageWrapper responseV24MessageWrapper)
	{
		this.responseV24MessageWrapper = responseV24MessageWrapper;
	}

	public String getMelding()
	{
		return melding;
	}

	public void setMelding(String melding)
	{
		this.melding = melding;
	}

	public Exception getCrashException()
	{
		return crashException;
	}

	public void setCrashException(Exception crashException)
	{
		this.crashException = crashException;
	}

	public boolean isSuccess()
	{
		return success;
	}

	public void setSuccess(boolean success)
	{
		this.success = success;
	}
}
