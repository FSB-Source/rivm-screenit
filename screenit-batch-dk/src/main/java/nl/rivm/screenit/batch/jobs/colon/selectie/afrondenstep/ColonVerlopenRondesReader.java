package nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.LogicalExpression;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class ColonVerlopenRondesReader extends BaseScrollableResultReader
{

	private static final int DAGEN_WACHTTIJD_CONCLUSIE = 5;

	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var currentDate = currentDateSupplier.getLocalDate();

		Date maxLeeftijd = DateUtil.toUtilDate(currentDate.minusYears(preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name()) + 1));
		Date maxLengteRonde = DateUtil.toUtilDate(currentDate.minusDays(preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name())));
		Date wachttijdNaAfspraak = DateUtil.toUtilDate(currentDate.minusDays(DAGEN_WACHTTIJD_CONCLUSIE));

		var criteria = session.createCriteria(ColonScreeningRonde.class, "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("ifobtTesten", "testen", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.laatsteUitnodiging", "uitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.laatsteBrief", "brief", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitnodiging.gekoppeldeTest", "ifobttest", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("afspraak.conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.lt("persoon.geboortedatum", maxLeeftijd));
		criteria.add(Restrictions.lt("ronde.creatieDatum", maxLengteRonde));
		criteria.add(Restrictions.or(
			Restrictions.and(Restrictions.isEmpty("ifobtTesten"), Restrictions.eq("brief.briefType", BriefType.COLON_UITNODIGING_ZONDER_FIT)),
			Restrictions.isNull("uitnodiging.id"),
			ifobtTestIsVerlopen(maxLengteRonde),
			Restrictions.isNull("afspraak.id"),
			conclusieNietBinnenWachtperiodeVerwerkt(wachttijdNaAfspraak),
			ColonRestrictions.critRondeZonderVerslagNaVerlopenOngunstigeUitslag(maxLengteRonde, null, null)));

		return criteria;
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
