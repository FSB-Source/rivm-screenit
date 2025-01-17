package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.repository.mamma.MammaDownloadOnderzoekenVerzoekRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_.AANGEMAAKT_OP;
import static nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_.GEDOWNLOAD_OP;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.filterId;
import static nl.rivm.screenit.specification.mamma.MammaDownloadOnderzoekenVerzoekSpecification.filterOpAangemaaktDoorGebruikerVanInstelling;
import static nl.rivm.screenit.specification.mamma.MammaDownloadOnderzoekenVerzoekSpecification.heeftGeenStatusIn;
import static org.springframework.data.domain.Sort.Direction;
import static org.springframework.data.domain.Sort.by;

@Service("MammaDownloadOnderzoekenVerzoekenDataProviderService")
public class MammaDownloadOnderzoekenVerzoekenDataProviderServiceImpl
	extends RepositoryDataProviderService<MammaDownloadOnderzoekenVerzoek, MammaDownloadOnderzoekenVerzoekRepository, MammaDownloadOnderzoekenVerzoek>
{
	@Override
	protected Specification<MammaDownloadOnderzoekenVerzoek> getSpecification(MammaDownloadOnderzoekenVerzoek filter, Sort sortParam)
	{
		var aangemaaktDoor = filter.getAangemaaktDoor();
		var bestandStatussen = filter.getStatus() != null ?
			List.of(BestandStatus.CRASH, BestandStatus.VERWERKT, BestandStatus.VERWIJDERD) :
			List.of(BestandStatus.VERWIJDERD);

		return filterOpAangemaaktDoorGebruikerVanInstelling(aangemaaktDoor != null ? aangemaaktDoor.getOrganisatie() : null).and(heeftGeenStatusIn(bestandStatussen))
			.and(filterId(filter.getId()));
	}

	@Override
	protected Sort getSort(Sort sort)
	{
		var gedownloadOpInSort = sort.stream().anyMatch(o -> o.getProperty().equals(GEDOWNLOAD_OP));
		return gedownloadOpInSort ? sort.and(by(Direction.DESC, AANGEMAAKT_OP)) : sort;
	}

}
