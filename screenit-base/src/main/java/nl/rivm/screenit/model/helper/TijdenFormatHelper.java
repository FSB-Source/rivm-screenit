package nl.rivm.screenit.model.helper;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.colon.planning.ITijdObject;

public final class TijdenFormatHelper
{

	private TijdenFormatHelper()
	{
	}

	public static String getTijden(ITijdObject tijdObject)
	{
		StringBuilder sb = new StringBuilder();

		sb.append(Constants.getTimeFormat().format(tijdObject.getStartTime()));
		sb.append(" - ");
		sb.append(Constants.getTimeFormat().format(tijdObject.getEndTime()));

		return sb.toString();
	}

	public static String getDatumTijden(ITijdObject tijdObject)
	{
		StringBuilder sb = new StringBuilder();

		sb.append(Constants.getDateTimeFormat().format(tijdObject.getStartTime()));
		sb.append(" - ");
		sb.append(Constants.getTimeFormat().format(tijdObject.getEndTime()));

		return sb.toString();
	}
}
