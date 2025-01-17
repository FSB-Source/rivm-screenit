package nl.rivm.screenit.batch.jobs.mamma.onderzoek.geenbeelden;

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
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus.NIET_BESCHIKBAAR;
import static nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus.AFGEROND;
import static nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus.ONVOLLEDIG;
import static nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption.MET_FOTOS;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftGeenId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaMammografieBaseSpecification.heeftIlmStatus;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftOnvolledigOnderzoek;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftStatus;

@Component
public class MammaOnderzoekenZonderBeeldenReader extends BaseSpecificationScrollableResultReader<MammaDossier>
{
	@Override
	protected Specification<MammaDossier> createSpecification()
	{
		return isAfgerondOfOnvolledigMetFotos().withRoot(this::laatsteOnderzoekJoin)
			.and(heeftGeenMammografieOfIlmStatusIsNietBeschikbaar().withRoot(this::laatsteMammografieJoin));
	}

	private ExtendedSpecification<MammaOnderzoek> isAfgerondOfOnvolledigMetFotos()
	{
		return heeftStatus(AFGEROND).or(isOnvolledigMetFotos());
	}

	private ExtendedSpecification<MammaOnderzoek> isOnvolledigMetFotos()
	{
		return heeftStatus(ONVOLLEDIG).and(heeftOnvolledigOnderzoek(MET_FOTOS));
	}

	private ExtendedSpecification<MammaMammografie> heeftGeenMammografieOfIlmStatusIsNietBeschikbaar()
	{
		return heeftIlmStatus(NIET_BESCHIKBAAR).or(heeftGeenId());
	}

	@Override
	protected Expression<Long> createProjection(Root<MammaDossier> r, CriteriaBuilder cb)
	{
		return laatsteAfspraakJoin(r).get(AbstractHibernateObject_.id);
	}

	private Join<?, MammaAfspraak> laatsteAfspraakJoin(Root<MammaDossier> root)
	{
		var laatsteScreeningRondeJoin = join(root, MammaDossier_.laatsteScreeningRonde);
		var laatsteUitnodigingJoin = join(laatsteScreeningRondeJoin, MammaScreeningRonde_.laatsteUitnodiging);
		return join(laatsteUitnodigingJoin, MammaUitnodiging_.laatsteAfspraak);
	}

	private Join<?, MammaOnderzoek> laatsteOnderzoekJoin(Root<MammaDossier> root)
	{
		var afspraakJoin = laatsteAfspraakJoin(root);
		return join(afspraakJoin, MammaAfspraak_.onderzoek);
	}

	private Join<?, MammaMammografie> laatsteMammografieJoin(Root<MammaDossier> root)
	{
		var onderzoekJoin = laatsteOnderzoekJoin(root);
		return join(onderzoekJoin, MammaOnderzoek_.mammografie, JoinType.LEFT);
	}
}
