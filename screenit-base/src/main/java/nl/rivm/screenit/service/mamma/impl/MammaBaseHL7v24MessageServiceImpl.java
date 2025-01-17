package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.repository.mamma.MammaHL7v24MessageRepository;
import nl.rivm.screenit.service.mamma.MammaBaseHL7v24MessageService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@RequiredArgsConstructor
public class MammaBaseHL7v24MessageServiceImpl implements MammaBaseHL7v24MessageService
{
	private final MammaHL7v24MessageRepository mammaHL7v24MessageRepository;

	@Transactional
	@Override
	public void verwijderBerichtVoorClient(Client client, boolean verwijderAlleBerichten)
	{
		if (verwijderAlleBerichten)
		{
			mammaHL7v24MessageRepository.verwijderAlleBerichtenVanClient(client.getId());
		}
		else
		{
			mammaHL7v24MessageRepository.verwijderAlleBerichtenExclusiefImsVoorClient(client.getId(), MammaHL7v24ORMBerichtStatus.DELETE.name(),
				MammaHL7v24ORMBerichtStatus.GOINGTODELETE.name());
		}
	}
}
