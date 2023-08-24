package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.afronden;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaVerlopenRondesReader extends BaseScrollableResultReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var maxLengteRonde = DateUtil.toUtilDate(vandaag.minusMonths(Constants.BK_GELDIGHEID_RONDE_MAANDEN));

		var criteria = session.createCriteria(MammaScreeningRonde.class, "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("ronde.laatsteUitnodiging", "uitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitnodiging.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.or(
			Restrictions.le("persoon.geboortedatum", maximaleGeboortedatum(vandaag)),
			Restrictions.eq("dossier.deelnamemodus", Deelnamemodus.SELECTIEBLOKKADE)
		));
		criteria.add(Restrictions.lt("ronde.creatieDatum", maxLengteRonde));
		criteria.add(Restrictions.isNull("ronde.laatsteOnderzoek"));
		criteria.add(Restrictions.or(
			Restrictions.isNull("afspraak.id"),
			Restrictions.and(
				Restrictions.isNotNull("afspraak.id"),
				Restrictions.or(
					Restrictions.ne("afspraak.status", MammaAfspraakStatus.GEPLAND),
					Restrictions.lt("afspraak.vanaf", DateUtil.toUtilDate(vandaag))))));

		return criteria;
	}

	private Date maximaleGeboortedatum(LocalDate currentDate)
	{
		var totEnMetLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		return DateUtil.toUtilDate(currentDate.minusYears(totEnMetLeeftijd + 1L));
	}
}
