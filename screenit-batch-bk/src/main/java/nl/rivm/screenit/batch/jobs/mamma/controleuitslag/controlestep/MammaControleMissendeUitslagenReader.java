package nl.rivm.screenit.batch.jobs.mamma.controleuitslag.controlestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class MammaControleMissendeUitslagenReader extends BaseSpecificationScrollableResultReader<MammaOnderzoek>
{
	private final MammaBaseOnderzoekService baseOnderzoekService;

	@Override
	protected Specification<MammaOnderzoek> createSpecification()
	{
		return baseOnderzoekService.getLaatsteOnderzoekMetMissendeUitslagSpecification();
	}

	@Override
	protected Expression<Long> createProjection(Root<MammaOnderzoek> r, CriteriaBuilder cb)
	{
		return dossierJoin(r).get(TablePerClassHibernateObject_.id);
	}

	@Override
	protected Order getOrder(Root<MammaOnderzoek> r, CriteriaBuilder cb)
	{
		return cb.asc(createProjection(r, cb));
	}

	private static Join<MammaScreeningRonde, MammaDossier> dossierJoin(Root<MammaOnderzoek> r)
	{
		var afspraakJoin = join(r, MammaOnderzoek_.afspraak);
		var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
		var screeningRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
		return join(screeningRondeJoin, MammaScreeningRonde_.dossier);
	}
}
