package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class ProjectBrievenAanmaakReader extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Date vandaag = currentDateSupplier.getDateMidnight();

		Criteria crit = session.createCriteria(ProjectBriefActie.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));

		crit.add( 
			Restrictions.or( 
				Restrictions.and(
					Restrictions.eq("type", ProjectBriefActieType.DATUM), 
					Restrictions.eq("datum", vandaag) 
		), 
				Restrictions.eq("type", ProjectBriefActieType.XDAGENNAY), 
				Restrictions.eq("type", ProjectBriefActieType.XMETY), 
				Restrictions.eq("type", ProjectBriefActieType.HERINNERING)) 
		); 

		crit.createAlias("project", "project");
		crit.add(
			Restrictions.and(
				Restrictions.gt("project.eindDatum", vandaag), 
				Restrictions.le("project.startDatum", vandaag)
		) 
		); 

		return crit;
	}
}
