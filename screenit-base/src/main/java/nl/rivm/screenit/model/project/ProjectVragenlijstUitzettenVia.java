package nl.rivm.screenit.model.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public enum ProjectVragenlijstUitzettenVia
{
	WEB,
	PAPIER,
	WEB_EN_PAPIER;

	public static boolean isPapier(ProjectVragenlijstUitzettenVia uitzettenVia)
	{
		return PAPIER.equals(uitzettenVia) || WEB_EN_PAPIER.equals(uitzettenVia);
	}

	public static boolean isWeb(ProjectVragenlijstUitzettenVia uitzettenVia)
	{
		return WEB.equals(uitzettenVia) || WEB_EN_PAPIER.equals(uitzettenVia);
	}
}
