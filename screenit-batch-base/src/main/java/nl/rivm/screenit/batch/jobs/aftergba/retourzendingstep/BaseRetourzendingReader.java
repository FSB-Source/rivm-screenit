package nl.rivm.screenit.batch.jobs.aftergba.retourzendingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.colon.enums.RetourzendingStatus;
import nl.rivm.screenit.specification.algemeen.GbaMutatieSpecification;
import nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

public abstract class BaseRetourzendingReader extends BaseSpecificationScrollableResultReader<Client>
{

	@Override
	protected Specification<Client> createSpecification()
	{
		return (r, q, cb) ->
		{
			var specification = GbaMutatieSpecification.heeftAanvullendeInformatieContaining("|" + getRetourzendingMarker() + "|").with(ri -> join(r, Client_.gbaMutaties))
				.and(InpakbareUitnodigingSpecification.heeftRetourzendingStatus(RetourzendingStatus.NIEUWE_GBA_ADRES_AANGEVRAAGD).with(ri -> getUitnodigingJoin(r)));
			return specification.toPredicate(r, q, cb);
		};
	}

	protected abstract String getRetourzendingMarker();

	protected abstract From<?, ? extends InpakbareUitnodiging<?>> getUitnodigingJoin(Root<Client> r);

	@Override
	protected Class<Client> getEntityClass()
	{
		return Client.class;
	}
}
