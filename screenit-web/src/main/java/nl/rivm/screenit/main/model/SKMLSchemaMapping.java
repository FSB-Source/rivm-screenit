package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

public class SKMLSchemaMapping
{

	private int skmljaar;

	private int skmlronde;

	private int monsterletter;

	private int deadline;

	public SKMLSchemaMapping()
	{
		setSkmljaar(-1);
		setSkmlronde(-1);
		setMonsterletter(-1);
		setDeadline(-1);
	}

	public int getSkmljaar()
	{
		return skmljaar;
	}

	public void setSkmljaar(int skmljaar)
	{
		this.skmljaar = skmljaar;
	}

	public int getSkmlronde()
	{
		return skmlronde;
	}

	public void setSkmlronde(int skmlronde)
	{
		this.skmlronde = skmlronde;
	}

	public int getMonsterletter()
	{
		return monsterletter;
	}

	public void setMonsterletter(int monsterletter)
	{
		this.monsterletter = monsterletter;
	}

	public int getDeadline()
	{
		return deadline;
	}

	public void setDeadline(int deadline)
	{
		this.deadline = deadline;
	}
}
