package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.model.LoginBrowserInfoDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static nl.rivm.screenit.security.UserAgentUtil.getParsedUserAgentInfo;

@RestController
@RequestMapping("login")
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
public class LoginController extends AbstractController
{
	private final HibernateService hibernateService;

	private final LogService logService;

	@Transactional(propagation = Propagation.REQUIRED)
	@PutMapping
	public ResponseEntity<Void> logLoggingInAction(@RequestBody LoginBrowserInfoDto loginBrowserInfo, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		logService.logGebeurtenis(LogGebeurtenis.INLOGGEN, client, getParsedUserAgentInfo(loginBrowserInfo.getUserAgent()));
		return ResponseEntity.ok().build();
	}

}
