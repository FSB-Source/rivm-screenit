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

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.followup.MammaFollowUpRadiologieInstellingOptieFilter;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.repository.mamma.MammaBaseFollowUpRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.filterLaatsteRadioVerslagenOpBeoordelingStatus;
import static nl.rivm.screenit.specification.mamma.MammaBaseFollowUpSpecification.heeftOpenstaandeRadioVerslagenVanInstelling;

@Service("MammaFollowUpRadiologieInstellingDataProviderService")
public class MammaFollowUpRadiologieInstellingDataProviderServiceImpl
	extends RepositoryDataProviderService<MammaFollowUpRadiologieVerslag, MammaBaseFollowUpRepository, MammaFollowUpRadiologieInstellingOptieFilter>
{
	@Override
	protected Specification<MammaFollowUpRadiologieVerslag> getSpecification(MammaFollowUpRadiologieInstellingOptieFilter filter, Sort sortParam)
	{
		var instelling = filter.getInstelling();
		var status = bepaalBeoordelingStatus(filter.getMammaFollowUpDoorverwezenFilterOptie());

		return heeftOpenstaandeRadioVerslagenVanInstelling(instelling).and(filterLaatsteRadioVerslagenOpBeoordelingStatus(status));
	}

	private MammaBeoordelingStatus bepaalBeoordelingStatus(MammaFollowUpDoorverwezenFilterOptie filter)
	{
		switch (filter)
		{
		case DOORVERWEZEN:
			return MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
		case NIET_DOORVERWEZEN:
			return MammaBeoordelingStatus.UITSLAG_GUNSTIG;
		default:
			return null;
		}
	}

}
