package nl.rivm.screenit.batch.jobs.colon.controleuitslag.controlestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
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
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftFitType;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftStatusDatumVoorOfOp;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.valideerFitUitslagStatus;

@Component
@AllArgsConstructor
public class ColonControleMissendeUitslagenReader extends BaseSpecificationScrollableResultReader<IFOBTTest>
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
			.and(heeftStatusDatumVoorOfOp(minimaleSignaleringsdatum.atStartOfDay()))
			.and(heeftFitType(IFOBTType.GOLD))
			.and(heeftActieveClient());
	}

	@Override
	protected Expression<Long> createProjection(Root<IFOBTTest> r, CriteriaBuilder cb)
	{
		return dossierAttribuut(r);
	}

	@Override
	protected Order getOrder(Root<IFOBTTest> r, CriteriaBuilder cb)
	{
		return cb.asc(createProjection(r, cb));
	}

	private static Path<Long> dossierAttribuut(Root<IFOBTTest> r)
	{
		return join(join(r, IFOBTTest_.colonScreeningRonde), ColonScreeningRonde_.dossier).get(TablePerClassHibernateObject_.id);
	}

}
