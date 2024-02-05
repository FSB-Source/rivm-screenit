package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.ColonUitnodigingsgebiedCapaciteitService;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

@Slf4j
public class ColonClientSelectieContext
{

	abstract class UitnodigingsTaak implements Cloneable
	{

		@Getter
		protected int clientenGeselecteerd = 0;

		void fetchMaxAantalClienten(long huidigeUitnodigingsGebiedId)
		{
		}

		abstract ColonUitnodigingCategorie getCategorie();

		boolean isMaxAantalClientenBereikt()
		{
			return false;
		}

		void clientGeselecteerd()
		{
			clientenGeselecteerd++;
		}

		public UitnodigingsTaak cloneTaak()
		{
			try
			{
				return (UitnodigingsTaak) this.clone();
			}
			catch (CloneNotSupportedException e)
			{
				throw new IllegalStateException(e);
			}
		}

		@Override
		public String toString()
		{
			return "UC=" + getCategorie() + ",";
		}

	}

	class VervolgrondeUitnodiging extends UitnodigingsTaak
	{

		@Override
		public ColonUitnodigingCategorie getCategorie()
		{
			return ColonUitnodigingCategorie.U2;
		}

		@Override
		public String toString()
		{
			return "[" + super.toString() + "vervolgrondes]";
		}

	}

	class EersteRondeUitnodiging extends UitnodigingsTaak
	{

		final int cohortJaar;

		EersteRondeUitnodiging(int cohortJaar)
		{
			this.cohortJaar = cohortJaar;
		}

		@Override
		public ColonUitnodigingCategorie getCategorie()
		{
			return ColonUitnodigingCategorie.U1;
		}

		@Override
		public String toString()
		{
			return "[" + super.toString() + "cj=" + cohortJaar + "]";
		}

	}
	class ProjectGroupUitnodiging extends UitnodigingsTaak
	{

		final long projectGroupId;

		private final ColonUitnodigingCategorie categorie;

		private int maxAantalClientenPerDag = 0;

		ProjectGroupUitnodiging(long projectGroupId, ColonUitnodigingCategorie categorie)
		{
			this.projectGroupId = projectGroupId;
			this.categorie = categorie;
		}

		@Override
		public void fetchMaxAantalClienten(long huidigeUitnodigingsGebiedId)
		{
			maxAantalClientenPerDag = uitnodigingsGebiedCapaciteitService.bepaalProjectGroepPopulatie(huidigeUitnodigingsGebiedId, categorie, projectGroupId, minimaleLeeftijd,
				maximaleLeeftijd);
		}

		@Override
		public ColonUitnodigingCategorie getCategorie()
		{
			return categorie;
		}

		@Override
		public boolean isMaxAantalClientenBereikt()
		{
			return clientenGeselecteerd == maxAantalClientenPerDag;
		}

		@Override
		public String toString()
		{
			return "[" + super.toString() + "gId='" + projectGroupId + "']";
		}

	}
	public final List<Long> exclusieGroepIds = Collections.synchronizedList(new ArrayList<>());

	public final List<Long> uitgenodigdeClientIds = Collections.synchronizedList(new ArrayList<>());

	public final List<UitnodigingsTaak> taken = Collections.synchronizedList(new ArrayList<>());

	public HibernateService hibernateService;

	public int fetchSize;

	public Integer minimaleLeeftijd;

	public Integer maximaleLeeftijd;

	public ColonUitnodigingsgebiedCapaciteitService uitnodigingsGebiedCapaciteitService;

	public ColonUitnodigingsDao uitnodigingsDao;

	public ClientDao clientDao;

	public IFobtService fitService;

	public void init(List<Integer> uitnodigingsJaren, List<ProjectGroep> projectGroepen)
	{
		addProjectGroepTaken(projectGroepen);
		taken.add(new VervolgrondeUitnodiging());
		for (var cohortJaar : uitnodigingsJaren)
		{
			taken.add(new EersteRondeUitnodiging(cohortJaar));
		}
		LOG.info("Taken: " + taken);
	}

	private void addProjectGroepTaken(List<ProjectGroep> projectGroepen)
	{
		for (var groep : projectGroepen)
		{
			taken.add(new ProjectGroupUitnodiging(groep.getId(), ColonUitnodigingCategorie.U2));
			LOG.info("Categorie: U2, Project {}/{} (groepId: '{}')", groep.getProject().getNaam(), groep.getNaam(), groep.getId());
			taken.add(new ProjectGroupUitnodiging(groep.getId(), ColonUitnodigingCategorie.U1));
			LOG.info("Categorie: U1, Project {}/{} (groepId: '{}')", groep.getProject().getNaam(), groep.getNaam(), groep.getId());
			exclusieGroepIds.add(groep.getId());
		}
	}
}
