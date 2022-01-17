package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.batch.service.ColonUitnodigingsgebiedCapaciteitService;
import nl.rivm.screenit.batch.service.impl.ColonUitnodigingsgebiedSelectieContext;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ColonClientSelectieContext
{

	private static final Logger LOG = LoggerFactory.getLogger(ColonClientSelectieContext.class);

	interface UitnodigingsTaak
	{
		Integer getMaxAantalClienten(ColonUitnodigingsgebiedSelectieContext huidigeUitnodigingsGebiedMetCapaciteit);
	}

	class CohortUitnodiging implements UitnodigingsTaak
	{

		final Integer cohortJaar;

		CohortUitnodiging(Integer cohortJaar)
		{
			this.cohortJaar = cohortJaar;
		}

		@Override
		public Integer getMaxAantalClienten(ColonUitnodigingsgebiedSelectieContext huidigeUitnodigingsGebiedMetCapaciteit)
		{
			return null; 
		}

		@Override
		public String toString()
		{
			return "[cJ=" + cohortJaar + "]";
		}

	}

	class ProjectGroupUitnodiging implements UitnodigingsTaak
	{

		Long projectGroupId;

		ProjectGroupUitnodiging(Long projectGroupId)
		{
			this.projectGroupId = projectGroupId;
		}

		@Override
		public Integer getMaxAantalClienten(ColonUitnodigingsgebiedSelectieContext huidigeUitnodigingsGebiedMetCapaciteit)
		{
			return uitnodigingsGebiedCapaciteitService.bepaalProjectGroepPopulatie(huidigeUitnodigingsGebiedMetCapaciteit.getUitnodigingsgebiedId(), projectGroupId,
				minimaleLeeftijd, maximaleLeeftijd);
		}

		@Override
		public String toString()
		{
			return "[gId=" + projectGroupId + "]";
		}

	}

	public final List<Long> exclusieGroepIds = Collections.synchronizedList(new ArrayList<>());

	public final List<Long> uitgenodigdeClientIds = Collections.synchronizedList(new ArrayList<>());

	public final List<UitnodigingsTaak> taken = Collections.synchronizedList(new ArrayList<>());

	HibernateService hibernateService;

	public int fetchSize;

	public int uitnodigingsInterval;

	public Integer minimaleLeeftijd;

	public Integer maximaleLeeftijd;

	public Integer wachttijdVerzendenPakket;

	public ColonUitnodigingsgebiedCapaciteitService uitnodigingsGebiedCapaciteitService;

	public ColonUitnodigingsDao uitnodigingsDao;

	public ClientDao clientDao;

	public void init(List<Integer> uitnodigingsJaren, List<ProjectGroep> projectGroepen)
	{
		for (ProjectGroep groep : projectGroepen)
		{
			taken.add(new ProjectGroupUitnodiging(groep.getId()));
			LOG.info("Project " + groep.getProject().getNaam() + "/" + groep.getNaam() + " (groepId:" + groep.getId() + ")");
			exclusieGroepIds.add(groep.getId());
		}
		for (Integer cohortJaar : uitnodigingsJaren)
		{
			taken.add(new CohortUitnodiging(cohortJaar));
		}
		LOG.info("Taken: " + taken);
	}

	Session getHibernateSession()
	{
		return hibernateService.getHibernateSession();
	}
}
