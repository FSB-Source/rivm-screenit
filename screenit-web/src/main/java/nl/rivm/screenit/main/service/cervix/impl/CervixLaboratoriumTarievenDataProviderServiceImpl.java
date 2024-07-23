package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.repository.cervix.CervixLabTariefRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftLabVoorTarief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isLabTariefActief;

@Service("cervixLaboratoriumTarievenDataProviderService")
public class CervixLaboratoriumTarievenDataProviderServiceImpl extends RepositoryDataProviderService<CervixLabTarief, CervixLabTariefRepository, BMHKLaboratorium>
{
	@Override
	protected Specification<CervixLabTarief> getSpecification(BMHKLaboratorium laboratorium, Sort sortParam)
	{
		return heeftLabVoorTarief(laboratorium).and(isLabTariefActief());
	}

}
