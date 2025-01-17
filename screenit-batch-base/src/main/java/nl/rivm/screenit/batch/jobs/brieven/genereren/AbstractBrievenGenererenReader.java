package nl.rivm.screenit.batch.jobs.brieven.genereren;

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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.organisatie.model.Adres_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftScreeningsOrganisatieId;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.isClientGekoppeldAanEenScreeningOrganisatie;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.magGegenereerdWorden;

public abstract class AbstractBrievenGenererenReader<B extends ClientBrief<?, ?, ?>> extends BaseSpecificationScrollableResultReader<B>
{
	protected abstract Long getScreeningOrganisatieId();

	@Override
	protected Specification<B> createSpecification()
	{
		var screeningOrganisatieId = getScreeningOrganisatieId();
		ExtendedSpecification<B> specification;
		if (screeningOrganisatieId != null)
		{
			specification = heeftScreeningsOrganisatieId(screeningOrganisatieId);
		}
		else
		{
			specification = isClientGekoppeldAanEenScreeningOrganisatie();
		}
		return specification.and(magGegenereerdWorden());
	}

	@Override
	protected Order getOrder(Root<B> r, CriteriaBuilder cb)
	{
		return cb.asc(adresJoin(r).get(Adres_.postcode));
	}

	@Override
	protected Class<?> getResultClass()
	{
		return Object[].class;
	}

	private Join<GbaPersoon, BagAdres> adresJoin(From<?, ? extends B> r)
	{
		var persoonJoin = join(clientJoin(r), Client_.persoon);
		return join(persoonJoin, GbaPersoon_.gbaAdres);
	}

	protected Join<? extends B, Client> clientJoin(From<?, ? extends B> r)
	{
		return join(r, ClientBrief_.client);
	}

	protected Join<BagAdres, Gemeente> gemeenteJoin(From<?, ? extends B> r)
	{
		var adresJoin = adresJoin(r);
		return join(adresJoin, BagAdres_.gbaGemeente);
	}
}
