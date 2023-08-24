package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaScreeningRondeEventReader extends BaseScrollableResultReader
{
	private final ICurrentDateSupplier dateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		var maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		var minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());

		var huidigJaar = dateSupplier.getLocalDate().getYear();
		var vanafGeboorteJaar = huidigJaar - maximaleLeeftijd - 1;
		var totEnMetGeboorteJaar = huidigJaar - minimaleLeeftijd + 3;

		criteria.add(Restrictions.ge("persoon.geboortedatum", DateUtil.toUtilDate(LocalDate.of(vanafGeboorteJaar, 1, 1))));
		criteria.add(Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(LocalDate.of(totEnMetGeboorteJaar, 12, 31))));

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");
		criteria.add(NvlRestrictions.eq("dossier.status", DossierStatus.ACTIEF, "'" + DossierStatus.ACTIEF.toString() + "'"));
		criteria.add(Restrictions.isNull("dossier.screeningRondeEvent"));

		return criteria;
	}

}
