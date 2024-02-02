package nl.rivm.screenit.batch.jobs.colon.selectie;

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

import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseTypedScrollableResultReader;
import nl.rivm.screenit.dao.colon.impl.ColonRestrictions;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
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
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class AbstractUitnodigingPushReader extends BaseTypedScrollableResultReader<ClientCategorieEntry>
{

	@Setter
	public static class ClientTePushenDto
	{
		private Long clientId;

		private Long projectGroepId;

		private Long gemeenteId;

		private Long screeningorganisatieId;
	}

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	protected ICurrentDateSupplier currentDateSupplier;

	private final ColonUitnodigingCategorie categorie;

	protected AbstractUitnodigingPushReader(ColonUitnodigingCategorie categorie)
	{
		this.categorie = categorie;
	}

	@Override
	public ClientCategorieEntry read() throws Exception
	{
		var scrollableResults = resultSet.get();
		while (scrollableResults.next())
		{
			var result = mapData(getObjectFromScrollableResult(scrollableResults));
			Long clientId = result.clientId;
			if (!processedIds.contains(clientId))
			{
				processedIds.add(clientId);
				Long screeningorganisatieId = controleerScreeningsorganisatie(result);
				if (screeningorganisatieId != null)
				{
					return new ClientCategorieEntry(clientId, categorie, screeningorganisatieId, result.projectGroepId, Boolean.TRUE);
				}
				else
				{

					return read();
				}
			}
		}
		return null;
	}

	private Long controleerScreeningsorganisatie(ClientTePushenDto result)
	{
		if (result.screeningorganisatieId != null)
		{
			return result.screeningorganisatieId;
		}
		var gemeente = hibernateService.get(Gemeente.class, result.gemeenteId);
		if (gemeente.getScreeningOrganisatie() == null)
		{
			var context = getExecutionContext();
			Map<Long, String> map = new HashMap<>();
			if (!context.containsKey(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES))
			{
				context.put(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES, map);
			}
			else
			{
				map = (Map<Long, String>) context.get(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES);
			}
			map.put(gemeente.getId(), gemeente.getNaam());
			LOG.warn("Gemeente {} (id: '{}') is niet gekoppeld aan een screeningsorganisatie. Daardoor kon er geen uitnodiging worden gepushed naar client (id:'{}')",
				gemeente.getNaam(), gemeente.getId(), result.clientId);
		}
		return null;
	}

	protected abstract Criteria createCriteria(StatelessSession session) throws HibernateException;

	protected final void createBaseCriteria(Criteria criteria)
	{
		var vandaag = currentDateSupplier.getLocalDate();
		criteria.createAlias("client.persoon", "persoon", JoinType.INNER_JOIN);
		criteria.createAlias("persoon.gbaAdres", "adres", JoinType.INNER_JOIN);
		criteria.createAlias("adres.gbaGemeente", "gemeente", JoinType.INNER_JOIN);
		criteria.createAlias("gemeente.screeningOrganisatie", "screeningorganisatie", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("client.colonDossier", "dossier", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("volgendeUitnodiging.interval", "interval", JoinType.LEFT_OUTER_JOIN);
		LocalDate peildatum = getPeildatum();
		if (categorie == ColonUitnodigingCategorie.U2)
		{
			criteria.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");
			criteria.createAlias("laatsteScreeningRonde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);
			criteria.add(ColonRestrictions.getU2BaseCriteria(peildatum, vandaag));
		}
		else
		{
			criteria.add(ColonRestrictions.getU1BaseCriteria(peildatum, null, vandaag));
		}
		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");
	}

	protected LocalDate getPeildatum()
	{
		return currentDateSupplier.getLocalDate();
	}

	@Override
	protected ScrollableResults createScrollableResults(StatelessSession session)
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
			.add(Projections.property("groep.id"))
			.add(Projections.property("gemeente.id"))
			.add(Projections.property("screeningorganisatie.id"));
	}

	protected ClientTePushenDto mapData(Object[] data)
	{
		var dto = new ClientTePushenDto();
		dto.setClientId(getNullSafeLongFromNumber(data[0]));
		dto.setGemeenteId(getNullSafeLongFromNumber(data[2]));
		dto.setProjectGroepId(getNullSafeLongFromNumber(data[1]));
		dto.setScreeningorganisatieId(getNullSafeLongFromNumber(data[3]));
		return dto;
	}

	protected static Long getNullSafeLongFromNumber(Object value)
	{
		var numberValue = (Number) value;
		return numberValue != null ? numberValue.longValue() : null;
	}
}
