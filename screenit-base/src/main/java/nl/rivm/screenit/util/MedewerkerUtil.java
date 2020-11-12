package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Session;

public class MedewerkerUtil
{
	
	private MedewerkerUtil()
	{
	}

	public static String getMedewerkerNamen(List<InstellingGebruiker> gebruikers)
	{
		StringBuilder sb = new StringBuilder();
		for (InstellingGebruiker gebruiker : gebruikers)
		{
			if (gebruiker != null && gebruiker.getMedewerker() != null)
			{
				sb.append(gebruiker.getMedewerker().getNaamVolledig());
				sb.append(", ");
			}
		}
		return StringUtils.removeEnd(sb.toString(), ", ").toString();
	}

	public static boolean isMedewerkerActief(Gebruiker medewerker, Date now)
	{
		return BooleanUtils.isNotFalse(medewerker.getActief())
			&& (medewerker.getActiefVanaf() == null || !DateUtil.compareAfter(medewerker.getActiefVanaf(), now))
			&& (medewerker.getActiefTotEnMet() == null || !DateUtil.compareBefore(medewerker.getActiefTotEnMet(), now));
	}

	public static String meldingNavActiefVanafEnTotEnMet(Gebruiker gebruiker, final Date now)
	{
		if (gebruiker != null && gebruiker.getActiefTotEnMet() != null && DateUtil.compareBefore(gebruiker.getActiefTotEnMet(), now))
		{
			return "Uw account is geïnactiveerd";
		}
		else if (gebruiker != null && gebruiker.getActiefVanaf() != null && DateUtil.compareAfter(gebruiker.getActiefVanaf(), now))
		{
			return "Uw account is nog niet actief";
		}
		else
		{
			return null;
		}
	}

	public static int getNextMedewerkercode(Session session)
	{
		return session.doReturningWork(new SequenceGenerator(DatabaseSequence.MEDEWERKERCODE, session.getSessionFactory())).intValue();
	}
}
