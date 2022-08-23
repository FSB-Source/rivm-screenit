package nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class CervixHerinnerenReader extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private InstellingService instellingService;

	private PreferenceKey periodeParameterKey;

	private OrganisatieParameterKey maxAantalParameterKey;

	private String periodeProperty;

	public CervixHerinnerenReader(int fetchSize, PreferenceKey periodeParameterKey, OrganisatieParameterKey maxAantalParameterKey, String periodeProperty)
	{
		super.setFetchSize(fetchSize);
		this.periodeParameterKey = periodeParameterKey;
		this.maxAantalParameterKey = maxAantalParameterKey;
		this.periodeProperty = periodeProperty;
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var crit = session.createCriteria(CervixUitnodiging.class, "uitnodiging");
		crit.createAlias("uitnodiging.screeningRonde", "ronde");
		crit.createAlias("ronde.dossier", "dossier");
		crit.createAlias("dossier.client", "client");
		crit.createAlias("client.persoon", "persoon");

		crit.createAlias("uitnodiging.brief", "brief");
		crit.createAlias("brief.mergedBrieven", "mergedBrieven");

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.eq("ronde.status", ScreeningRondeStatus.LOPEND));

		crit.add(Restrictions.eq("uitnodiging.herinneren", true));
		crit.add(Restrictions.isNull("uitnodiging.herinnerenGeannuleerdDatum"));

		crit.add(Restrictions.lt(periodeProperty, getMaxPeriodeDatum(periodeParameterKey)));

		if (maxAantalParameterKey == OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_ZAS)
		{
			crit.createAlias("uitnodiging.monster", "zas", JoinType.INNER_JOIN, Restrictions.eq("uitnodiging.monsterType", CervixMonsterType.ZAS));
		}
		else
		{
			crit.createAlias("uitnodiging.monster", "uitstrijkje", JoinType.INNER_JOIN, Restrictions.eq("uitnodiging.monsterType", CervixMonsterType.UITSTRIJKJE));
		}

		Integer maxAantalHerinneringen = getMaxAantalHerinneringen(maxAantalParameterKey);
		if (maxAantalHerinneringen != null)
		{
			crit.setMaxResults(maxAantalHerinneringen);
			crit.addOrder(Order.asc(periodeProperty));
		}

		return crit;
	}

	@Override
	protected Projection getProjection()
	{
		Integer maxAantalHerinneringen = getMaxAantalHerinneringen(maxAantalParameterKey);
		if (maxAantalHerinneringen != null)
		{
			return Projections.distinct(Projections.projectionList().add(Projections.id()).add(Projections.property(periodeProperty)));
		}
		return super.getProjection();
	}

	private Date getMaxPeriodeDatum(PreferenceKey preferenceKey)
	{
		Integer preferenceValue = preferenceService.getInteger(preferenceKey.name());
		return DateUtil.toUtilDate(dateSupplier.getLocalDate().minusWeeks(preferenceValue).plusDays(1));
	}

	private Integer getMaxAantalHerinneringen(OrganisatieParameterKey paramKey)
	{
		return instellingService.getOrganisatieParameter(null, paramKey);
	}

}
