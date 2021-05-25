package nl.rivm.screenit.model.mamma.enums;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.INaam;

public enum MammaBIRADSWaarde implements INaam
{
	NUL("0", 3),
	EEN("1", 1),
	TWEE("2", 2),
	VIER("4", 5),
	VIJF("5", 6),
	GEEN("Geen", 0);

	private String naam;

	private int prio;

	MammaBIRADSWaarde(String naam, int prio)
	{
		this.naam = naam;
		this.prio = prio;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public int getPrio()
	{
		return prio;
	}

	public static List<MammaBIRADSWaarde> getNietVerwijzendBIRADSWaarden()
	{
		return Arrays.asList(MammaBIRADSWaarde.GEEN, MammaBIRADSWaarde.EEN, MammaBIRADSWaarde.TWEE);
	}

	public static List<MammaBIRADSWaarde> getVerwijzendBIRADSWaarden()
	{
		return Arrays.asList(MammaBIRADSWaarde.NUL, MammaBIRADSWaarde.VIER, MammaBIRADSWaarde.VIJF);
	}

	public static MammaBIRADSWaarde getHoogsteBiradsUitTweeWaarden(MammaBIRADSWaarde waarde1, MammaBIRADSWaarde waarde2)
	{
		return waarde1.getPrio() > waarde2.getPrio() ? waarde1 : waarde2;
	}
}
