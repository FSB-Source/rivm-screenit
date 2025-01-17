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

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;

import org.apache.commons.collections.CollectionUtils;

@Getter
@NoArgsConstructor
public class BeperkingException extends Exception
{
	private final List<ValidatieException> exceptions = new ArrayList<>();

	@Setter
	private ColonRoosterBeperking beperkingType;

	public void addException(ValidatieException exception)
	{
		exceptions.add(exception);
	}

	@Override
	public boolean equals(Object obj)
	{
		var other = (BeperkingException) obj;
		if (other == null)
		{
			return false;
		}

		if (getClass() != obj.getClass())
		{
			return false;
		}

		if (!other.getBeperkingType().equals(getBeperkingType()))
		{
			return false;
		}

		return CollectionUtils.isEqualCollection(other.getExceptions(), getExceptions());
	}

	@Override
	public int hashCode()
	{
		return beperkingType.hashCode() + exceptions.hashCode();
	}
}
