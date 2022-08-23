package nl.rivm.screenit.batch.jobs.colon.selectie.uitnodingingpushstep;

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

import java.util.HashMap;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseTypedScrollableResultReader;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.CriteriaImpl;
import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class AbstractUitnodigingPushReader extends BaseTypedScrollableResultReader<ClientCategorieEntry>
{

	private static final String CLIENTID = "clientId";

	private static final String CLIENTBSN = "clientBsn";

	private static final String GROEPID = "projectGroepId";

	private static final String GEMEENTEID = "gemeenteId";

	private static final String SOID = "screeningorganisatieId";

	@Autowired
	private HibernateService hibernateService;

	protected ColonUitnodigingCategorie categorie;

	public AbstractUitnodigingPushReader(ColonUitnodigingCategorie categorie)
	{
		this.categorie = categorie;
	}

	@Override
	public ClientCategorieEntry read() throws Exception
	{
		var scrollableResults = resultSet.get();
		while (scrollableResults.next())
		{
			var results = getMapWithData(getObjectFromScrollableResult(scrollableResults));
			Long clientId = ((Number) results.get(CLIENTID)).longValue();
			Long projectGroepId = ((Number) results.get(GROEPID)).longValue();
			if (!processedIds.contains(clientId))
			{
				processedIds.add(clientId);
				Long screeningorganisatieId = controleerScreeningsorganisatie(results);
				if (screeningorganisatieId != null)
				{
					return new ClientCategorieEntry(clientId, categorie, screeningorganisatieId, projectGroepId, Boolean.TRUE);
				}
				else
				{

					return read();
				}
			}
		}
		return null;
	}

	private Long controleerScreeningsorganisatie(Map<String, Object> dataMap)
	{
		if (dataMap.containsKey(SOID))
		{
			return ((Number) dataMap.get(SOID)).longValue();
		}
		Long gemeenteId = ((Number) dataMap.get(GEMEENTEID)).longValue();
		if (gemeenteId != null)
		{
			String bsn = String.valueOf(dataMap.get(CLIENTBSN));
			var gemeente = hibernateService.get(Gemeente.class, gemeenteId);
			if (gemeente.getScreeningOrganisatie() == null)
			{
				var context = getExecutionContext();
				if (!context.containsKey(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES))
				{
					Map<Long, String> map = new HashMap<Long, String>();
					context.put(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES, map);
				}
				Map<Long, String> map = (Map<Long, String>) context.get(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES);
				map.put(gemeente.getId(), gemeente.getNaam());
				context.put(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES, map);
				LOG.warn("Gemeente " + gemeente.getNaam() + " (" + gemeente.getId()
					+ ") niet is gekoppeld aan een screeningsorganisatie. Daardoor kon er geen uitnodiging worden gepushed naar client(" + bsn
					+ ")");
			}
		}
		return null;
	}

	public abstract Criteria createCriteria(StatelessSession session) throws HibernateException;

	@Override
	public ScrollableResults createScrollableResults(StatelessSession session)
	{
		var crit = createCriteria(session);

		if (Integer.valueOf(0).equals(((CriteriaImpl) crit).getMaxResults()))
		{
			crit.add(Restrictions.sqlRestriction("1 = 0"));
		}
		return crit.setFetchSize(fetchSize).setProjection(getProjection()).scroll(ScrollMode.FORWARD_ONLY);
	}

	private Object[] getObjectFromScrollableResult(ScrollableResults scrollableResults)
	{
		return scrollableResults.get();
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.projectionList()
			.add(Projections.property("client.id"))
			.add(Projections.property("persoon.bsn"))
			.add(Projections.property("groep.id"))
			.add(Projections.property("gemeente.id"))
			.add(Projections.property("screeningorganisatie.id"));
	}

	private Map<String, Object> getMapWithData(Object[] data)
	{
		Map<String, Object> map = new HashMap<>();
		map.put(CLIENTID, data[0]);
		map.put(CLIENTBSN, data[1]);
		map.put(GROEPID, data[2]);
		map.put(GEMEENTEID, data[3]);
		if (data[4] != null)
		{
			map.put(SOID, data[4]);
		}
		return map;
	}
}
