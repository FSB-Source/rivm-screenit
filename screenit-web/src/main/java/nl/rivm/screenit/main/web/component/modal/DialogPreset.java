package nl.rivm.screenit.main.web.component.modal;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

public enum DialogPreset
{

	WIJZIG_AFSPRAAK(true, 810, 650),

	NIEUWE_AFSPRAAK(true, 400, 400),

	VARIABELLEN_OVERZICHT(true, 660, 550),

	MEDIUM_MOD(true, 390, 310);

	private final boolean modal;

	private final int width;

	private final int height;

	private DialogPreset(boolean modal, int width, int height)
	{
		this.modal = modal;
		this.width = width;
		this.height = height;
	}

	public boolean isModal()
	{
		return modal;
	}

	public int getWidth()
	{
		return width;
	}

	public int getHeight()
	{
		return height;
	}
}
