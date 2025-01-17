package nl.rivm.screenit.service.impl;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class BaseClientContactServiceImpl implements BaseClientContactService
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void verwijderClientContacten(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		if (CollectionUtils.isNotEmpty(client.getContacten()))
		{
			List<Bevolkingsonderzoek> onderzoeken = new ArrayList<Bevolkingsonderzoek>();
			onderzoeken.add(bevolkingsonderzoek);
			List<ClientContact> contacten = client.getContacten();
			for (ClientContact contact : contacten)
			{
				boolean clientContactMagWeg = true;
				for (ClientContactActie actie : contact.getActies())
				{
					if (onderzoeken.equals(actie.getType().getBevolkingsonderzoeken()))
					{
						hibernateService.delete(actie);
					}
					else
					{
						clientContactMagWeg = false;
					}
				}
				if (clientContactMagWeg)
				{
					hibernateService.delete(contact);
				}
			}
		}
	}
}
