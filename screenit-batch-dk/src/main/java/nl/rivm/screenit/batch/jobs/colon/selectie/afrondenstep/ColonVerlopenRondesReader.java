package nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.util.Date;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonClientSelectieHelper;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.LogicalExpression;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public class ColonVerlopenRondesReader extends BaseScrollableResultReader
{

	private static final int DAGEN_WACHTTIJD_CONCLUSIE = 5;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		LocalDate currentDate = currentDateSupplier.getLocalDate();

		Date maxLeeftijd = DateUtil.toUtilDate(currentDate.minusYears(preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name())));
		Date maxLengteRonde = DateUtil.toUtilDate(currentDate.minusDays(preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name())));
		Date wachttijdNaAfspraak = DateUtil.toUtilDate(currentDate.minusDays(DAGEN_WACHTTIJD_CONCLUSIE));

		Criteria criteria = session.createCriteria(ColonScreeningRonde.class, "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("ifobtTesten", "testen");
		criteria.createAlias("ronde.laatsteUitnodiging", "uitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitnodiging.gekoppeldeTest", "ifobttest", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("afspraak.conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.lt("persoon.geboortedatum", maxLeeftijd));
		criteria.add(Restrictions.lt("ronde.creatieDatum", maxLengteRonde));
		criteria.add(Restrictions.or(
			Restrictions.or(
				Restrictions.isNull("uitnodiging.id"),
				ifobtTestIsVerlopen(maxLengteRonde)),
			Restrictions.or(
				Restrictions.isNull("afspraak.id"),
				Restrictions.or(
					conclusieNietBinnenWachtperiodeVerwerkt(wachttijdNaAfspraak),
					rondeZonderVerslagNaVerlopenOngunstigeUitslag(maxLengteRonde)))));

		return criteria;
	}

	private LogicalExpression rondeZonderVerslagNaVerlopenOngunstigeUitslag(Date maxLengteRonde)
	{
		return Restrictions.and(
			Subqueries.propertiesIn(new String[] { "testen.statusDatum", "id" }, ColonClientSelectieHelper.critEersteOngunstigeUitslagUitLaatsteRonde()),
			Restrictions.le("testen.statusDatum", maxLengteRonde));
	}

	private Conjunction conclusieNietBinnenWachtperiodeVerwerkt(Date wachttijdNaAfspraak)
	{
		return Restrictions.and(
			Restrictions.isNull("afspraak.conclusie"),
			Restrictions.eq("afspraak.status", AfspraakStatus.GEPLAND),
			Restrictions.lt("afspraak.startTime", wachttijdNaAfspraak));
	}

	private LogicalExpression ifobtTestIsVerlopen(Date maxLengteRonde)
	{
		return Restrictions.and(
			Restrictions.eq("ifobttest.status", IFOBTTestStatus.ACTIEF),
			Restrictions.lt("uitnodiging.creatieDatum", maxLengteRonde));
	}
}
