package nl.rivm.screenit.main.service.mamma.impl;

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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.repository.mamma.MammaPostcodeReeksRepository;
import nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.mamma.MammaPostcodeReeksSpecification.filterOpPostcode4OfPostcode6;
import static nl.rivm.screenit.specification.mamma.MammaPostcodeReeksSpecification.heeftStandplaats;

@Service("mammaPostcodeReeksDataProvideService")
@RequiredArgsConstructor
public class MammaPostcodeReeksDataProviderServiceImpl extends RepositoryDataProviderService<MammaPostcodeReeks, MammaPostcodeReeksRepository, MammaPostcodeReeks>
{
	@Override
	protected Specification<MammaPostcodeReeks> getSpecification(MammaPostcodeReeks filter, Sort sortParam)
	{
		var vanPostcode = filter.getVanPostcode();
		return getSpecificationVanuitStandplaats(filter.getStandplaats()).and(filterOpPostcode4OfPostcode6(vanPostcode));
	}

	private Specification<MammaPostcodeReeks> getSpecificationVanuitStandplaats(MammaStandplaats standplaats)
	{
		if (standplaats != null)
		{
			if (standplaats.getId() != null)
			{
				return heeftStandplaats(standplaats);
			}
			else if (standplaats.getRegio() != null)
			{
				return MammaStandplaatsSpecification.heeftRegio(standplaats.getRegio()).with(MammaPostcodeReeks_.standplaats);
			}
		}
		return (r, q, cb) -> null;
	}
}
