package nl.rivm.screenit.batch.jobs.generalis.gba.wrappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

public class GbaValidatieWrapper
{
	private boolean clientIsVerwijderd;

	private boolean stopVerwerking;

	public GbaValidatieWrapper(boolean clientIsVerwijderd, boolean stopVerwerking)
	{
		this.clientIsVerwijderd = clientIsVerwijderd;
		this.stopVerwerking = stopVerwerking;
	}

	public static GbaValidatieWrapper stopVerwerking(boolean clientIsVerwijderd)
	{
		return new GbaValidatieWrapper(clientIsVerwijderd, true);
	}

	public boolean isClientIsVerwijderd()
	{
		return clientIsVerwijderd;
	}

	public boolean isStopVerwerking()
	{
		return stopVerwerking;
	}
}
