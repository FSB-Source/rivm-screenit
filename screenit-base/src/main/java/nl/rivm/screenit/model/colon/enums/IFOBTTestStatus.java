package nl.rivm.screenit.model.colon.enums;

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

import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.util.IFOBTTestUtil;

public enum IFOBTTestStatus
{

	ACTIEF
	{
		@Override
		public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
		{
			if (!buis.getType().equals(IFOBTType.STUDIE))
			{
				return Arrays.asList(VERLOREN, UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, buis);
			}
			else
			{
				return Arrays.asList(VERLOREN, UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN, VERWIJDERD).contains(nieuweStatus);
			}
		}
	},

	VERLOREN
	{
		@Override
		public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
		{
			return Arrays.asList(UITGEVOERD).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, buis);
		}
	},

	DOETNIETMEE
	{
		@Override
		public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
		{
			return Arrays.asList(UITGEVOERD).contains(nieuweStatus) && IFOBTTestUtil.isOngunstig(buis);
		}
	},

	WACHTOPTEST,

	WACHTOPTEST_VERWIJDERD_AF,

	WELBRIEFGEENTEST,

	NIETONTVANGEN,

	NIETTEBEOORDELEN
	{
		@Override
		public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
		{
			return Arrays.asList(UITGEVOERD).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, buis);
		}
	},

	VERVALDATUMVERLOPEN
	{
		@Override
		public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
		{
			return Arrays.asList(VERWIJDERD).contains(nieuweStatus);
		}
	},

	WACHTOPBRIEF,

	ONBETROUWBAAR,

	WELTESTGEENBRIEF,

	UITGEVOERD
	{
		@Override
		public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
		{
			return Arrays.asList(VERWIJDERD).contains(nieuweStatus);
		}
	},

	VERWIJDERD,

	TB_PAIRED_WACHTOPTEST,

	TB_PAIRED_WACHTOPBRIEF,

	TB_P_TWEEDE_WACHTOPTEST,

	TB_P_TWEEDE_WACHTOPBRIEF,

	;

	private static final IFOBTTestStatus[] MUTABLE_EIND_STATUSSEN = new IFOBTTestStatus[] { VERVALDATUMVERLOPEN, NIETTEBEOORDELEN, UITGEVOERD, VERLOREN };

	public static final IFOBTTestStatus[] UNMUTABLE_EIND_STATUSSEN = new IFOBTTestStatus[] { VERWIJDERD };

	public boolean magWijzigenNaarStatus(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
	{
		return false;
	}

	private static boolean uitgevoerdEnUitslag(IFOBTTestStatus nieuweStatus, IFOBTTest buis)
	{
		return nieuweStatus != UITGEVOERD || IFOBTTestUtil.isOngunstig(buis) || IFOBTTestUtil.isGunstig(buis);
	}

	public static boolean isMutableEindStatus(IFOBTTestStatus status)
	{
		return Arrays.asList(MUTABLE_EIND_STATUSSEN).contains(status);
	}

	public static boolean isUnmutableEindStatus(IFOBTTestStatus status)
	{
		return Arrays.asList(UNMUTABLE_EIND_STATUSSEN).contains(status);
	}

}
