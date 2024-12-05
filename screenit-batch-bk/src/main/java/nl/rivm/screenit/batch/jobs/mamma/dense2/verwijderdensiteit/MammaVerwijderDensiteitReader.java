package nl.rivm.screenit.batch.jobs.mamma.dense2.verwijderdensiteit;

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
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
public class MammaVerwijderDensiteitReader extends BaseSpecificationScrollableResultReader<Client>
{
	@Autowired
	private MammaBaseDense2Service dense2Service;

	@Override
	public Specification<Client> createSpecification()
	{
		return dense2Service.getClientenSpecificationVoorVerwijderenDensiteit();
	}

	@Override
	protected Expression<Long> createProjection(Root<Client> r, CriteriaBuilder cb)
	{
		var dossierJoin = join(r, Client_.mammaDossier);
		var rondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
		return rondeJoin.get(MammaScreeningRonde_.laatsteOnderzoek).get(AbstractHibernateObject_.id);
	}
}
