package nl.rivm.screenit.batch.jobs.colon.controleuitslag.controlestep;

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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationSortableScrollableResultReader;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftFitType;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.isStatusDatumVoorOfOp;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.valideerFitUitslagStatus;

@Component
@AllArgsConstructor
public class ColonControleMissendeUitslagenReader extends BaseSpecificationSortableScrollableResultReader<IFOBTTest, Object>
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	protected Specification<IFOBTTest> createSpecification()
	{
		var signaleringstermijn = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.COLON_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30);
		var vandaag = currentDateSupplier.getLocalDate();
		var signalerenVanaf = vandaag.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN);
		var minimaleSignaleringsdatum = vandaag.minusDays(signaleringstermijn);
		return valideerFitUitslagStatus(signalerenVanaf)
			.and(isStatusDatumVoorOfOp(minimaleSignaleringsdatum))
			.and(heeftFitType(IFOBTType.GOLD))
			.and(heeftActieveClient());
	}

	@Override
	protected CriteriaQuery<Object> createProjection(Root<IFOBTTest> r, CriteriaQuery<Object> q, CriteriaBuilder cb)
	{
		return q.select(r.get(IFOBTTest_.colonScreeningRonde).get(ColonScreeningRonde_.dossier).get(TablePerClassHibernateObject_.id)).distinct(true);
	}

	@Override
	protected Order getOrder(Root<IFOBTTest> r, CriteriaBuilder cb)
	{
		return cb.asc(r.get(IFOBTTest_.colonScreeningRonde).get(ColonScreeningRonde_.dossier).get(TablePerClassHibernateObject_.id));
	}

	@Override
	protected Class<Object> getResultClass()
	{
		return Object.class;
	}
}
