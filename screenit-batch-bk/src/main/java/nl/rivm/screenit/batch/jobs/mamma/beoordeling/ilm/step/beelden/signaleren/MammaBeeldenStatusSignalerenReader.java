package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.signaleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaBeeldenStatusSignalerenReader extends BaseScrollableResultReader
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var signaleerTermijn = DateUtil.toUtilDate(
			currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_SIGNALEERTERMIJN_BEELDEN_STATUS.name())));

		var criteria = session.createCriteria(MammaMammografie.class);

		criteria.add(Restrictions.in("ilmStatus", MammaMammografieIlmStatus.TE_VERWIJDEREN, MammaMammografieIlmStatus.VERWIJDEREN_MISLUKT));
		criteria.add(Restrictions.lt("ilmStatusDatum", signaleerTermijn));

		return criteria;
	}
}
