package nl.rivm.screenit.util.query;

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

import java.time.LocalDate;

import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Restrictions;

public class ScreenitRestrictions
{

	private ScreenitRestrictions()
	{
	}

	public static String fixAlias(String alias)
	{
		if (StringUtils.isNotBlank(alias) && !alias.endsWith("."))
		{
			alias += ".";
		}
		return alias;
	}

	public static Conjunction getLeeftijdsgrensRestrictions(Integer minLeeftijd, Integer maxLeeftijd, LocalDate peildatum)
	{
		return getLeeftijdsgrensRestrictions(minLeeftijd, maxLeeftijd, null, peildatum);
	}

	@Deprecated(forRemoval = true)
	public static Conjunction getLeeftijdsgrensRestrictions(Integer minLeeftijd, Integer maxLeeftijd, Integer interval, LocalDate peildatum)
	{
		Conjunction conjunction = Restrictions.conjunction();
		if (minLeeftijd != null)
		{
			LocalDate geboortedatumMaximaal = peildatum.minusYears(minLeeftijd);
			conjunction.add(Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMaximaal)));
		}
		if (maxLeeftijd != null)
		{
			LocalDate geboortedatumMinimaal = peildatum.minusYears(maxLeeftijd + 1L);
			if (interval != null)
			{
				geboortedatumMinimaal = geboortedatumMinimaal.minusDays(interval);
			}
			conjunction.add(Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal)));
		}
		return conjunction;
	}

	public static void addClientBaseRestrictions(Criteria crit, String clientAlias, String persoonAlias)
	{
		clientAlias = fixAlias(clientAlias);
		persoonAlias = fixAlias(persoonAlias);

		crit.add(Restrictions.eq(clientAlias + "gbaStatus", GbaStatus.INDICATIE_AANWEZIG));

		addPersoonBaseRestrictions(crit, persoonAlias);
	}

	public static String getClientBaseRestrictions(String clientAlias, String persoonAlias)
	{
		return clientAlias + ".gba_status = '" + GbaStatus.INDICATIE_AANWEZIG + "' and "
			+ getPersoonBaseRestrictions(persoonAlias);
	}

	public static void addPersoonBaseRestrictions(Criteria crit, String persoonAlias)
	{
		persoonAlias = fixAlias(persoonAlias);

		crit.add(Restrictions.isNull(persoonAlias + "overlijdensdatum"));

		crit.add(Restrictions.isNull(persoonAlias + "datumVertrokkenUitNederland"));
	}

	public static String getPersoonBaseRestrictions(String persoonAlias)
	{
		return persoonAlias + ".overlijdensdatum is null"
			+ " and " + persoonAlias + ".datum_vertrokken_uit_nederland is null";
	}

}
