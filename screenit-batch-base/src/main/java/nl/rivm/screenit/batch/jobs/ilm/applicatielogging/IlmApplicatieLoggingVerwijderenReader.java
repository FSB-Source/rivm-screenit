package nl.rivm.screenit.batch.jobs.ilm.applicatielogging;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.Date;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.persister.collection.CollectionPropertyNames;
import org.springframework.beans.factory.annotation.Autowired;

public class IlmApplicatieLoggingVerwijderenReader extends BaseScrollableResultReader
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private Bevolkingsonderzoek bvo;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Date wachttijd = DateUtil
			.toUtilDate(currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN_NIET_MEDISCH.name())));

		Criteria criteria = session.createCriteria(LogRegel.class);
		criteria.add(Restrictions.lt("gebeurtenisDatum", wachttijd));
		if (bvo == null)
		{
			criteria.add(Restrictions.sizeNe("bevolkingsonderzoeken", 1));
		}
		else
		{
			criteria.createAlias("bevolkingsonderzoeken", "bevolkingsonderzoeken");
			criteria.add(Restrictions.sizeEq("bevolkingsonderzoeken", 1));
			criteria.add(Restrictions.eq("bevolkingsonderzoeken." + CollectionPropertyNames.COLLECTION_ELEMENTS, bvo));
		}
		return criteria;
	}

	public void setBvo(Bevolkingsonderzoek bvo)
	{
		this.bvo = bvo;
	}
}
