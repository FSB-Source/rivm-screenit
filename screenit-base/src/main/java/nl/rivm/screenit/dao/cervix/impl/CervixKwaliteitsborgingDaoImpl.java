package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.dao.cervix.CervixKwaliteitsborgingDao;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixKwaliteitsborgingDaoImpl extends AbstractAutowiredDao implements CervixKwaliteitsborgingDao
{

	@Override
	public Long getNextKwaliteitsborgingBarcode()
	{
		return getSession().doReturningWork(new SequenceGenerator(DatabaseSequence.HPTV_KWALITEITSBORGING_ID, getSessionFactory()));
	}
}
