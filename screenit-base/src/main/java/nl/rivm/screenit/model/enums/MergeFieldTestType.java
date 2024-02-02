package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public enum MergeFieldTestType
{
	CLIENT,

	ZORGINSTELLING,

	ZORGVERLENER,

	INTAKE,

	INTAKELOCATIE(true),

	BMHKLAB(true),

	BMHKHUISARTS,

	BKAFSPRAAK,

	BKSTANDPLAATS(true),

	BKRADIOLOOG(false, true),

	OVERIGE;

	private final boolean fromDB;

	private final boolean freeText;

	MergeFieldTestType(boolean fromDB, boolean freeText)
	{
		this.fromDB = fromDB;
		this.freeText = freeText;
	}

	MergeFieldTestType(boolean fromDB)
	{
		this(fromDB, false);
	}

	MergeFieldTestType()
	{
		this(false);
	}

	public boolean isFromDB()
	{
		return fromDB;
	}

	public boolean isFreeText()
	{
		return freeText;
	}

}
