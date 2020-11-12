package nl.rivm.screenit.util.query;

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

import java.time.LocalDate;

import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;

public class ScreenitRestrictions
{

	private ScreenitRestrictions()
	{
	}

	public static Criterion colonOngunstig()
	{
		return colonOngunstig("");
	}

	public static Criterion colonOngunstig(String alias)
	{
		alias = fixAlias(alias);
		return Restrictions.geProperty(alias + "uitslag", alias + "normWaarde");
	}

	public static void addNogGeenColonUitslagbriefOntvangenCriteria(Criteria crit, String rondeAlias)
	{
		rondeAlias = fixAlias(rondeAlias);
		DetachedCriteria subquery = DetachedCriteria.forClass(ColonBrief.class, "brief");
		subquery.setProjection(Projections.id());
		subquery.add(Restrictions.eqProperty("brief.screeningRonde", rondeAlias + "id"));
		subquery.add(Restrictions.in("brief.briefType", BriefType.COLON_UITSLAG_BRIEVEN));
		crit.add(Subqueries.notExists(subquery));
	}

	private static String fixAlias(String alias)
	{
		if (StringUtils.isNotBlank(alias) && !alias.endsWith("."))
		{
			alias += ".";
		}
		return alias;
	}

	public static Criterion colonGunstig(String alias)
	{
		alias = fixAlias(alias);
		return Restrictions.ltProperty(alias + "uitslag", alias + "normWaarde");
	}

	public static void addExcludeProjectClientenMetProjectStatusNogTeStarten(Criteria crit)
	{
		crit.add(Restrictions.sqlRestriction("{alias}.wacht_op_start_project = false"));
	}

	public static void addLeeftijdsGrensRestrictions(Criteria crit, Integer minLeeftijd, Integer maxLeeftijd, LocalDate pijlDatum)
	{
		addLeeftijdsgrensRestrictions(crit, minLeeftijd, maxLeeftijd, null, pijlDatum);
	}

	public static void addLeeftijdsgrensRestrictions(Criteria crit, Integer minLeeftijd, Integer maxLeeftijd, Integer interval, LocalDate pijlDatum)
	{
		Conjunction conjunction = Restrictions.conjunction();
		if (minLeeftijd != null)
		{
			LocalDate geboortedatumMaximaal = pijlDatum.minusYears(minLeeftijd);
			conjunction.add(Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMaximaal)));
		}
		if (maxLeeftijd != null)
		{
			LocalDate geboortedatumMinimaal = pijlDatum.minusYears(maxLeeftijd + 1);
			if (interval != null)
			{
				geboortedatumMinimaal = geboortedatumMinimaal.minusDays(interval);
			}
			conjunction.add(Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal)));
		}
		if (conjunction.conditions().iterator().hasNext())
		{
			crit.add(conjunction);
		}
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
		return clientAlias + ".gba_status = \'" + GbaStatus.INDICATIE_AANWEZIG + "\' and "
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

	public static void addHeeftGeenAfgerondeColonVerlagenRestrictions(Criteria criteria, String rondeAlias)
	{
		DetachedCriteria verslagenCrit = DetachedCriteria.forClass(ColonVerslag.class);
		verslagenCrit.add(Restrictions.eq("status", VerslagStatus.AFGEROND));
		verslagenCrit.setProjection(Projections.distinct(Projections.property("screeningRonde.id")));
		criteria.add(Subqueries.propertyNotIn(rondeAlias + "id", verslagenCrit));
	}
}
