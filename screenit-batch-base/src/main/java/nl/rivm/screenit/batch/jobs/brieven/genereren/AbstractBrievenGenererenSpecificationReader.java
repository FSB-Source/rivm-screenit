package nl.rivm.screenit.batch.jobs.brieven.genereren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.organisatie.model.Adres_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isNietGegenereerd;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isNietTegengehouden;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isNietVervangen;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftVervangendeProjectBrief;
import static nl.rivm.screenit.specification.algemeen.GemeenteSpecification.heeftScreeningOrganisatie;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isNietOverledenEnWoontInNederland;

public abstract class AbstractBrievenGenererenSpecificationReader<B extends ClientBrief<?, ?, ?>> extends BaseSpecificationScrollableResultReader<B>
{
	protected abstract Long getScreeningOrganisatieId();

	@Override
	protected Specification<B> createSpecification()
	{
		var screeningOrganisatieId = getScreeningOrganisatieId();
		ExtendedSpecification<B> specification;
		if (screeningOrganisatieId != null)
		{
			specification = heeftId(screeningOrganisatieId).with(r -> screeningsorganisatieJoin(r));
		}
		else
		{
			specification = heeftScreeningOrganisatie().with(r -> gemeenteJoin(r));
		}
		return specification
			.and(isNietOverledenEnWoontInNederland().with(r -> persoonJoin(r)))
			.and(isNietGegenereerd())
			.and(isNietVervangen())
			.and(isNietTegengehouden())
			.and(heeftVervangendeProjectBrief(false));
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

	private Join<Gemeente, ScreeningOrganisatie> screeningsorganisatieJoin(From<?, ? extends B> r)
	{
		return join(gemeenteJoin(r), Gemeente_.screeningOrganisatie);
	}

	private Join<BagAdres, Gemeente> gemeenteJoin(From<?, ? extends B> r)
	{
		return join(adresJoin(r), BagAdres_.gbaGemeente);
	}

	private Join<GbaPersoon, BagAdres> adresJoin(From<?, ? extends B> r)
	{
		return join(persoonJoin(r), GbaPersoon_.gbaAdres);
	}

	private Join<Client, GbaPersoon> persoonJoin(From<?, ? extends B> r)
	{
		return join(clientJoin(r), Client_.persoon);
	}

	protected Join<? extends B, Client> clientJoin(From<?, ? extends B> r)
	{
		return join(r, ClientBrief_.client);
	}
}
