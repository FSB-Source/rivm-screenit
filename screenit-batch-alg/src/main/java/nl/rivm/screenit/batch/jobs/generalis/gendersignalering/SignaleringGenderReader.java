package nl.rivm.screenit.batch.jobs.generalis.gendersignalering;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class SignaleringGenderReader extends BaseScrollableResultReader
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(Client.class, "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("mammaDossier", "mammaDossier", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("cervixDossier", "cervixDossier", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("client.algemeneBrieven", "signaleringsbrief", JoinType.LEFT_OUTER_JOIN,
			Restrictions.eq("signaleringsbrief.briefType", BriefType.CLIENT_SIGNALERING_GENDER));

		criteria.add(Restrictions.isNull("signaleringsbrief.id"));

		criteria.add(Restrictions.or(
			Restrictions.eq("mammaDossier.deelnamemodus", Deelnamemodus.SELECTIEBLOKKADE),
			Restrictions.eq("cervixDossier.deelnamemodus", Deelnamemodus.SELECTIEBLOKKADE)));

		criteria.add(Restrictions.gt("persoon.geboortedatum", maxGeboorteDatumVoorDoelgroep()));

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		return criteria;
	}

	private Date maxGeboorteDatumVoorDoelgroep()
	{
		var mammaTotEnMetLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		var cervixTotEnMetLeeftijd = CervixLeeftijdcategorie._70.getLeeftijd();
		var signalerenTotLeeftijd = Math.max(mammaTotEnMetLeeftijd, cervixTotEnMetLeeftijd) + 1;
		var vanafGeboorteDatum = currentDateSupplier.getLocalDate().minusYears(signalerenTotLeeftijd);
		return DateUtil.toUtilDate(vanafGeboorteDatum);
	}

}
