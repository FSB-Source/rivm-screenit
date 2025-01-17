package nl.rivm.screenit.batch.jobs.cervix.vervolgonderzoek.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.function.Function;

import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isLopend;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftControleUitstrijkjeDatumVoorOfOp;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftGeenUitnodigingVervolgonderzoek;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftGeenUitstel;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftInVervolgonderzoekDatum;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftInVervolgonderzoekDatumVoorOntvangstDatumOfScanDatum;
import static nl.rivm.screenit.specification.cervix.CervixUitstelSpecification.heeftGeannuleerdDatum;
import static org.springframework.data.jpa.domain.Specification.where;

@Component
public class CervixVervolgonderzoekReader extends BaseSpecificationScrollableResultReader<CervixScreeningRonde>
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	public CervixVervolgonderzoekReader()
	{
		super.setFetchSize(50);
	}

	@Override
	protected Specification<CervixScreeningRonde> createSpecification()
	{
		return (r, q, cb) ->
		{
			var subquery = heeftInVervolgonderzoekDatumVoorOntvangstDatumOfScanDatum(q, cb);

			return where(heeftActieveClient().with(clientJoin()))
				.and(isLopend())
				.and(heeftInVervolgonderzoekDatum())
				.and(heeftGeenUitnodigingVervolgonderzoek())
				.and(where(heeftGeenUitstel())
					.or(heeftGeannuleerdDatum().with(uitstelJoin())))
				.and(heeftControleUitstrijkjeDatumVoorOfOp(dateSupplier.getLocalDate()))
				.and((subRoot, subQuery, subBuilder) ->
					subBuilder.not(subRoot.get(TablePerClassHibernateObject_.id).in(subquery))
				)
				.toPredicate(r, q, cb);
		};
	}

	private static Function<From<?, ? extends CervixScreeningRonde>, From<?, ? extends CervixDossier>> cervixDossierJoin()
	{
		return q -> join(q, CervixScreeningRonde_.dossier);
	}

	private static Function<From<?, ? extends CervixScreeningRonde>, From<?, ? extends Client>> clientJoin()
	{
		return q ->
		{
			var dossierJoin = cervixDossierJoin().apply(q);
			return join(dossierJoin, CervixDossier_.client);
		};
	}

	private static Function<From<?, ? extends CervixScreeningRonde>, From<?, ? extends CervixUitstel>> uitstelJoin()
	{
		return q -> join(q, CervixScreeningRonde_.uitstel, JoinType.LEFT);
	}
}
