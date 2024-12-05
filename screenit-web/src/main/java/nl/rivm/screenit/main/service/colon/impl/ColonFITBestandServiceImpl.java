package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;

import javax.persistence.criteria.Predicate;

import nl.rivm.screenit.main.model.colon.IFobtBatchFilter;
import nl.rivm.screenit.main.service.colon.ColonFITBestandService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTBestand_;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFOBTUitslag_;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonFITBestandRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.RangeUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.colon.ColonFITBestandSpecification.filterLaboratorium;
import static nl.rivm.screenit.specification.colon.ColonFITBestandSpecification.filterStatus;
import static nl.rivm.screenit.specification.colon.ColonFITBestandSpecification.heeftStatusDatumTussen;
import static nl.rivm.screenit.specification.colon.ColonFITUitslagSpecification.heeftAnalyseDatumTussen;

@Service
public class ColonFITBestandServiceImpl implements ColonFITBestandService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonFITBestandRepository fitBestandRepository;

	@Override
	public List<IFOBTBestand> getBestanden(IFobtBatchFilter filter, long first, long count, Sort sort)
	{
		if (isRangeInvalide(filter))
		{
			return List.of();
		}

		return fitBestandRepository.findWith(getBestandenSpecification(filter), q -> q.sortBy(sort, (order, r, cb) ->
		{
			var sortProperty = order.getProperty();
			if (sortProperty.startsWith(IFOBTBestand_.LABORATORIUM))
			{
				join(r, IFOBTBestand_.laboratorium);
			}
			return null;
		})).all(first, count);
	}

	@Override
	public long countBestanden(IFobtBatchFilter filter)
	{
		if (isRangeInvalide(filter))
		{
			return 0;
		}

		return fitBestandRepository.count(getBestandenSpecification(filter));
	}

	private Specification<IFOBTBestand> getBestandenSpecification(IFobtBatchFilter filter)
	{
		return (r, q, cb) ->
		{

			var predicates = new ArrayList<Predicate>();
			predicates.add(filterStatus(filter.getStatus()).and(filterLaboratorium(filter.getLab())).toPredicate(r, q, cb));
			var datumVan = filter.getDatumVan() != null ? DateUtil.startDag(filter.getDatumVan()) : null;
			var datumTot = filter.getDatumTot() != null ? DateUtil.plusDagen(DateUtil.startDag(filter.getDatumTot()), 1) : null;

			var range = RangeUtil.range(datumVan, BoundType.CLOSED, datumTot, BoundType.OPEN);
			if (filter.isAnalyseDatum())
			{
				var subquery = q.subquery(Long.class);
				var subRoot = subquery.from(IFOBTUitslag.class);
				subquery.select(subRoot.get(IFOBTUitslag_.bestand).get(AbstractHibernateObject_.id))
					.where(heeftAnalyseDatumTussen(range).toPredicate(subRoot, q, cb));
				predicates.add(cb.in(r.get(AbstractHibernateObject_.id)).value(subquery));

			}
			else
			{
				predicates.add(heeftStatusDatumTussen(range).toPredicate(r, q, cb));
			}
			return composePredicates(cb, predicates);
		};
	}

	private boolean isRangeInvalide(IFobtBatchFilter filter)
	{
		return filter.getDatumVan() != null && filter.getDatumTot() != null && filter.getDatumVan().after(filter.getDatumTot());
	}

	@Override
	@Transactional
	public void verwijderBestanden(List<IFOBTBestand> ifobtBestanden, Account ingelogdeAccount)
	{
		IFobtLaboratorium lab = null;
		for (var bestand : ifobtBestanden)
		{
			bestand.setStatus(IFOBTBestandStatus.VERWIJDERD);
			lab = bestand.getLaboratorium();
			hibernateService.saveOrUpdate(bestand);
		}
		if (lab != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_BESTANDEN_VERWIJDERD, ingelogdeAccount, "Labid qbase " + lab.getQbasenummer(), Bevolkingsonderzoek.COLON);
		}
	}

	@Override
	@Transactional
	public void autoriseerBestanden(List<IFOBTBestand> ifobtBestanden, Account ingelogdeAccount)
	{
		IFobtLaboratorium lab = null;
		for (var bestand : ifobtBestanden)
		{
			lab = bestand.getLaboratorium();
			bestand.setStatus(IFOBTBestandStatus.GEAUTORISEERD);
			hibernateService.saveOrUpdate(bestand);
		}
		if (lab != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_BESTANDEN_GEAUTORISEERD, ingelogdeAccount, "Labid qbase " + lab.getQbasenummer(), Bevolkingsonderzoek.COLON);
		}
	}
}
