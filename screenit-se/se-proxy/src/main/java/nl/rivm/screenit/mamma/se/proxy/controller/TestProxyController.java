package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.servlet.http.HttpSession;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/test")
public class TestProxyController
{
	@Autowired
	private SeRestSocketService socketService;

	@Autowired
	private SeStatusService statusService;

	private static final Logger LOG = LoggerFactory.getLogger(TestProxyController.class);

	@Autowired
	private ProxyService proxyService;

	@RequestMapping(value = "/offline", method = RequestMethod.POST)
	public ResponseEntity<String> gaOffline(HttpSession httpSession)
	{
		if (!SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
		{
			LOG.warn("Omgeving is niet Test en er is een request uitgevoerd om de SE offline te halen.");
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}

		statusService.setTestOnline(false);
		socketService.closeSocket();
		return ResponseEntity.ok().build();
	}

	@RequestMapping(value = "/online", method = RequestMethod.POST)
	public ResponseEntity<String> gaOnline(HttpSession httpSession)
	{
		if (!SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
		{
			LOG.warn("Omgeving is niet Test en er is een request uitgevoerd om de SE online te zetten.");
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}

		statusService.setTestOnline(true);
		return ResponseEntity.ok().build();
	}
}
