package nl.rivm.screenit.batch.jobs.mamma.onderzoek.nietafgesloten;

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
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification;
import nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
public class MammaOnderzoekenNietAfgeslotenReader extends BaseSpecificationScrollableResultReader<MammaDossier>
{
	@Override
	protected Specification<MammaDossier> createSpecification()
	{
		return MammaAfspraakSpecification.heeftStatus(MammaAfspraakStatus.INGESCHREVEN).withRoot(this::laatsteAfspraakJoin)
			.or(MammaOnderzoekSpecification.isDoorgevoerd(false).withRoot(this::onderzoekJoin));
	}

	@Override
	protected Expression<Long> createProjection(Root<MammaDossier> r, CriteriaBuilder cb)
	{
		return laatsteAfspraakJoin(r).get(AbstractHibernateObject_.id);
	}

	private Join<MammaUitnodiging, MammaAfspraak> laatsteAfspraakJoin(Root<MammaDossier> dossierRoot)
	{
		var laatsteScreeningRondeJoin = join(dossierRoot, MammaDossier_.laatsteScreeningRonde);
		var laatsteUitnodigingJoin = join(laatsteScreeningRondeJoin, MammaScreeningRonde_.laatsteUitnodiging);
		return join(laatsteUitnodigingJoin, MammaUitnodiging_.laatsteAfspraak);
	}

	private Join<MammaAfspraak, MammaOnderzoek> onderzoekJoin(Root<MammaDossier> dossierRoot)
	{
		var laatsteAfspraakJoin = laatsteAfspraakJoin(dossierRoot);
		return join(laatsteAfspraakJoin, MammaAfspraak_.onderzoek, JoinType.LEFT);
	}
}
