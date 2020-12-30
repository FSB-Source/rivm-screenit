
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.CorrectieService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CorrectieServiceImpl implements CorrectieService
{

	private static final Logger LOG = LoggerFactory.getLogger(CorrectieServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void clearHibernateCache()
	{
		SessionFactory sf = hibernateService.getHibernateSession().getSessionFactory();
		sf.getCache().evictEntityRegions();
		sf.getCache().evictCollectionRegions();
		sf.getCache().evictDefaultQueryRegion();
		sf.getCache().evictQueryRegions();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void opruimenGefaaldeBatchJobs()
	{
		Query query = hibernateService.getHibernateSession()
			.createSQLQuery("UPDATE gedeeld.batch_job_execution SET status = 'COMPLETED', end_time= now() where status = 'STARTED'");
		query.executeUpdate();
	}

}
