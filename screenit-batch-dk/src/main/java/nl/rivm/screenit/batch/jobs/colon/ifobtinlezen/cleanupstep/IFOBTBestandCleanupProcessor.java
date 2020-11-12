
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.cleanupstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;

public class IFOBTBestandCleanupProcessor implements ItemProcessor<Long, IFOBTBestand>
{
	
	@Autowired
	private HibernateService hibernateService;

	@Override
	public IFOBTBestand process(Long item) throws Exception
	{
		return hibernateService.load(IFOBTBestand.class, item);
	}

}
