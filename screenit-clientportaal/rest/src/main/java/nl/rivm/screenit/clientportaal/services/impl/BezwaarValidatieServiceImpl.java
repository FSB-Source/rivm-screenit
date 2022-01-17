package nl.rivm.screenit.clientportaal.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.clientportaal.model.BezwaarDto;
import nl.rivm.screenit.clientportaal.services.BezwaarValidatieService;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ClientDoelgroepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class BezwaarValidatieServiceImpl implements BezwaarValidatieService
{
	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Override
	public void valideerOfClientBehoortTotDoelgroep(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		if (!doelgroepService.behoortTotDoelgroep(client, bevolkingsonderzoek))
		{
			throw new IllegalStateException("De cliënt behoort niet tot de doelgroep");
		}
	}

	@Override
	public void valideerBezwaarType(BezwaarDto[] bezwaarDtos)
	{
		for (BezwaarDto bezwaarDto : bezwaarDtos)
		{
			if (bezwaarDto.getType().getOnzichtbaarOpClientPortaal())
			{
				throw new IllegalStateException("Bezwaar niet beschikbaar");
			}
		}
	}
}
