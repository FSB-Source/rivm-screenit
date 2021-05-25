package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventReader;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaScreeningRondeEventReader extends MammaAbstractEventReader
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	protected Criteria getCriteria(StatelessSession session)
	{
		Criteria criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		Integer maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		Integer minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());

		int huidigJaar = dateSupplier.getLocalDate().getYear();
		int vanafGeboorteJaar = huidigJaar - maximaleLeeftijd - 1;
		int totEnMetGeboorteJaar = huidigJaar - minimaleLeeftijd + 3;

		criteria.add(Restrictions.ge("persoon.geboortedatum", DateUtil.toUtilDate(LocalDate.of(vanafGeboorteJaar, 1, 1))));
		criteria.add(Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(LocalDate.of(totEnMetGeboorteJaar, 12, 31))));

		criteria.add(Restrictions.eq("persoon.geslacht", Geslacht.VROUW));
		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");
		criteria.add(NvlRestrictions.eq("dossier.status", DossierStatus.ACTIEF, "'" + DossierStatus.ACTIEF.toString() + "'"));
		criteria.add(Restrictions.isNull("dossier.screeningRondeEvent"));

		return criteria;
	}
}
