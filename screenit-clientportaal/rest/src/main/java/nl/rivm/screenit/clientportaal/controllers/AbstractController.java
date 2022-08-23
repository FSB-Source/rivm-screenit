package nl.rivm.screenit.clientportaal.controllers;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.filter.CPAccountResolverDelegate;
import nl.rivm.screenit.clientportaal.security.userdetails.ScreenitUserDetails;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.service.ClientContactService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;

@Slf4j
public abstract class AbstractController
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientContactService clientContactService;

	public Client getClient(Authentication authentication)
	{
		Client client = hibernateService.get(Client.class, ((ScreenitUserDetails) authentication.getPrincipal()).getClientId());
		CPAccountResolverDelegate.setClient(client);
		return client;
	}

	protected boolean aanvraagIsToegestaneActie(Client client, ClientContactActieType clientContactActieType)
	{
		return clientContactService.availableActiesBevatBenodigdeActie(client, clientContactActieType);
	}

	protected <T> ResponseEntity<T> createForbiddenResponse()
	{
		LOG.error("Gebruiker is niet geautoriseerd om de actie uit te voeren");
		return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
	}
}
