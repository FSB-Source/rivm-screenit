package nl.rivm.screenit.batch.jobs.mamma.aftergba.imswijzigingendoorsturen;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER;
import static nl.rivm.screenit.Constants.MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER;
import static nl.rivm.screenit.batch.jobs.mamma.aftergba.AfterGbaJobConfiguration.AFTER_GBA_JOB_READER_FETCH_SIZE;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftMammaDossier;
import static nl.rivm.screenit.specification.algemeen.GbaMutatieSpecification.heeftAanvullendeInformatieContaining;

@Component
public class MammaImsWijzigingenDoorsturenReader extends BaseSpecificationScrollableResultReader<Client>
{

	public MammaImsWijzigingenDoorsturenReader()
	{
		super.setFetchSize(AFTER_GBA_JOB_READER_FETCH_SIZE);
	}

	@Override
	protected Specification<Client> createSpecification()
	{
		return heeftMutatieMarker(MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER).or(
			heeftMutatieMarker(MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER)).and(heeftMammaDossier());
	}

	private Specification<Client> heeftMutatieMarker(String marker)
	{
		return heeftAanvullendeInformatieContaining(marker).with(r -> join(r, Client_.gbaMutaties));
	}
}
