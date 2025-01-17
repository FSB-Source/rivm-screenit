package nl.rivm.screenit.batch.jobs.mamma.onderzoek.onderbrokenonderzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isAangemaaktVoor;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isDoorgevoerd;

@Component
@AllArgsConstructor
public class MammaVervolgTeOudeOnderbrokenOnderzoekenReader extends BaseSpecificationScrollableResultReader<MammaDossier>
{

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MammaDossier> createSpecification()
	{
		return heeftStatus(MammaOnderzoekStatus.ONDERBROKEN)
			.and(isDoorgevoerd(true))
			.and(isAangemaaktVoor(currentDateSupplier.getLocalDate().minusMonths(6).atStartOfDay()))
			.with(r -> laatsteOnderZoekJoin(r));

	}

	@Override
	protected Expression<Long> createProjection(Root<MammaDossier> r, CriteriaBuilder cb)
	{
		return laatsteOnderZoekJoin(r).get(AbstractHibernateObject_.id);
	}

	private static Join<MammaScreeningRonde, MammaOnderzoek> laatsteOnderZoekJoin(From<?, ? extends MammaDossier> dossierRoot)
	{
		var rondeJoin = join(dossierRoot, MammaDossier_.laatsteScreeningRonde);
		return join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
	}
}
