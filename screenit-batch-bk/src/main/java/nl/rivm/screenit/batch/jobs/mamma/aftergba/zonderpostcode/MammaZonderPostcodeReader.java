package nl.rivm.screenit.batch.jobs.mamma.aftergba.zonderpostcode;

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

import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.specification.algemeen.AdresSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.mamma.aftergba.AfterGbaJobConfiguration.AFTER_GBA_JOB_READER_FETCH_SIZE;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
public class MammaZonderPostcodeReader extends BaseSpecificationScrollableResultReader<Client>
{

	public MammaZonderPostcodeReader()
	{
		super.setFetchSize(AFTER_GBA_JOB_READER_FETCH_SIZE);
	}

	@Override
	protected Specification<Client> createSpecification()
	{
		return ClientSpecification.heeftActieveClient()
			.and(ClientSpecification.heeftMammaDossier())
			.and(heeftGemeenteMetScreeningsOrganisatie())
			.and(heeftGeenGbaAdresMetPostcode())
			.and(heeftGeenTijdelijkGbaAdresMetPostcode());
	}

	private Specification<Client> heeftGemeenteMetScreeningsOrganisatie()
	{
		return GemeenteSpecification.heeftScreeningOrganisatie().with(r ->
		{
			var persoonJoin = join(r, Client_.persoon);
			var adresJoin = join(persoonJoin, GbaPersoon_.gbaAdres);
			return join(adresJoin, BagAdres_.gbaGemeente);
		});
	}

	private Specification<Client> heeftGeenGbaAdresMetPostcode()
	{
		return AdresSpecification.heeftGeenPostcode().with(r ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return join(persoonJoin, GbaPersoon_.gbaAdres);
		});
	}

	private Specification<Client> heeftGeenTijdelijkGbaAdresMetPostcode()
	{
		return AdresSpecification.heeftGeenPostcode().with(r ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return join(persoonJoin, GbaPersoon_.tijdelijkGbaAdres, JoinType.LEFT);
		});
	}
}
