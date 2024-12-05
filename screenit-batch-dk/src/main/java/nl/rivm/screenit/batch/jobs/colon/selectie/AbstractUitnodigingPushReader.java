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

import java.lang.reflect.ParameterizedType;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import lombok.AllArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseTypedScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.repository.impl.FluentJpaQueryImpl;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification;

import org.hibernate.ScrollableResults;
import org.hibernate.StatelessSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Slf4j
public abstract class AbstractUitnodigingPushReader<T> extends BaseTypedScrollableResultReader<ClientCategorieEntry>
{

	@Setter
	@AllArgsConstructor
	public static class ClientTePushenDto
	{
		private Long clientId;

		private Long projectGroepId;

		private Long gemeenteId;

		private Long screeningorganisatieId;
	}

	@Autowired
	private GemeenteRepository gemeenteRepository;

	@Autowired
	protected ICurrentDateSupplier currentDateSupplier;

	private final ColonUitnodigingCategorie categorie;

	protected AbstractUitnodigingPushReader(ColonUitnodigingCategorie categorie)
	{
		this.categorie = categorie;
	}

	@Override
	public ClientCategorieEntry read()
	{
		var scrollableResults = resultSet.get();
		while (scrollableResults.next())
		{
			var result = (ClientTePushenDto) scrollableResults.get()[0];
			Long clientId = result.clientId;
			if (!processedIds.contains(clientId))
			{
				processedIds.add(clientId);
				var screeningorganisatieId = controleerScreeningsorganisatie(result);
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
		var gemeente = gemeenteRepository.findById(result.gemeenteId).orElseThrow();
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

	protected final ExtendedSpecification<Client> baseSpecifications()
	{
		ExtendedSpecification<Client> specification;
		var vandaag = currentDateSupplier.getLocalDate();
		var peildatum = getPeildatum();
		if (categorie == ColonUitnodigingCategorie.U2)
		{
			specification = ColonUitnodigingBaseSpecification.u2Base(peildatum, vandaag, JoinType.INNER).with(r -> join(r, Client_.colonDossier));
		}
		else
		{
			specification = ColonUitnodigingBaseSpecification.u1Base(peildatum, vandaag, null);
		}
		return specification.and(ClientSpecification.heeftActieveClient());
	}

	protected LocalDate getPeildatum()
	{
		return currentDateSupplier.getLocalDate();
	}

	@Override
	protected ScrollableResults createScrollableResults(StatelessSession session)
	{
		var jpaQuery = new FluentJpaQueryImpl<>(createSpecification(), getHibernateSession(), getEntityClass(), ClientTePushenDto.class);
		jpaQuery.projections((cb, r) -> createProjections(r, cb));

		return jpaQuery.setScrollFetchSize(fetchSize).scroll(-1);
	}

	protected abstract Specification<T> createSpecification();

	protected abstract List<Selection<?>> createProjections(Root<T> r, CriteriaBuilder cb);

	protected Class<T> getEntityClass()
	{
		return (Class<T>) ((ParameterizedType) ((Class<?>) getClass().getGenericSuperclass()).getGenericSuperclass()).getActualTypeArguments()[0];
	}

}
