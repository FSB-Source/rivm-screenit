package nl.rivm.screenit.batch.jobs.mamma.herinneren.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.BriefType;
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
import org.springframework.beans.factory.annotation.Autowired;

public class MammaHerinnerenReader extends BaseScrollableResultReader
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(MammaScreeningRonde.class, "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("screeningRonde.laatsteUitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.brief", "brief");
		criteria.createAlias("uitnodiging.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningRonde.laatsteUitstel", "uitstel", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("screeningRonde.status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.eq("screeningRonde.minderValideOnderzoekZiekenhuis", false));
		criteria.add(Restrictions.eq("dossier.status", DossierStatus.ACTIEF));
		criteria.add(Restrictions.isNull("dossier.tehuis"));
		criteria.add(Restrictions.eq("uitnodiging.herinnered", false));
		criteria.add(Restrictions.or(
			Restrictions.isNull("uitstel.id"), 
			Restrictions.isNotNull("uitstel.geannuleerdOp"), 
			Restrictions.isNotNull("uitstel.uitnodiging") 
		));

		criteria.add(Restrictions.or(
			Restrictions.and(
				Restrictions.eq("afspraak.status", MammaAfspraakStatus.GEPLAND),
				Restrictions.le("afspraak.vanaf", getMaxNoshowPeriode()) 
			), 
			Restrictions.and(
				Restrictions.isNull("afspraak.id"),
				Restrictions.ne("brief.briefType", BriefType.MAMMA_UITNODIGING_SUSPECT), 
				Restrictions.le("uitnodiging.creatieDatum", getMaxGeenAfspraakPeriode()) 
			) 
		));

		return criteria;
	}

	private Date getMaxGeenAfspraakPeriode()
	{
		Integer herinneringsPeriodeGeenAfspraak = preferenceService.getInteger(PreferenceKey.MAMMA_HERINNERINGS_PERIODE_GEEN_AFSPRAAK.name(), Integer.valueOf(4));
		return DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusWeeks(herinneringsPeriodeGeenAfspraak));
	}

	private Date getMaxNoshowPeriode()
	{
		Integer herinneringsPeriodeNoShow = preferenceService.getInteger(PreferenceKey.MAMMA_HERINNERINGS_PERIODE_NO_SHOW.name(), Integer.valueOf(2));
		return DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusWeeks(herinneringsPeriodeNoShow));
	}
}
