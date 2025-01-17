package nl.rivm.screenit.main.service.cervix.impl;

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

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.filterLabVoorBoekRegel;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.heeftNietVerrichtingType;

@Service("cervixLabBoekregelsDataProviderService")
public class CervixLabBoekregelsDataProviderServiceImpl extends AbstractCervixBoekregelsDataProviderServiceImpl
{
	@Override
	protected Specification<CervixBoekRegel> getSpecification(CervixVerrichtingenZoekObject filter, Sort sortParam)
	{
		return super.getSpecification(filter, sortParam)
			.and(filterLabVoorBoekRegel(filter.getBmhkLaboratorium()))
			.and(heeftNietVerrichtingType(CervixTariefType.HUISARTS_UITSTRIJKJE));
	}
}
