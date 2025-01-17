package nl.rivm.screenit.main.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.colon.planning.ColonTijdslot;

@Getter
@AllArgsConstructor
public class BulkAanmakenInnerException extends Exception
{
	private final Exception exception;

	private final ColonTijdslot tijdslot;

	@Override
	public boolean equals(Object obj)
	{
		var other = (BulkAanmakenInnerException) obj;
		if (other == null)
		{
			return false;
		}

		if (getClass() != obj.getClass())
		{
			return false;
		}

		if (!tijdslot.toString().equalsIgnoreCase(other.tijdslot.toString()))
		{
			return false;
		}

		return exception.equals(other.exception);
	}

	@Override
	public int hashCode()
	{
		return tijdslot.hashCode() + exception.hashCode();
	}
}
