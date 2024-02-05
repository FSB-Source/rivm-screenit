package nl.rivm.screenit.wsb.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import javax.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.handler.CervixHpvHL7v251Handler;
import nl.rivm.screenit.handler.ColonIFobtHL7v251Handler;
import nl.rivm.screenit.handler.MammaHL7v24Handler;
import nl.rivm.screenit.wsb.service.ScreenITHL7v2ServerService;
import nl.rivm.screenit.wsb.service.mamma.MammaBeeldenOntvangenService;
import nl.rivm.screenit.wsb.service.mamma.MammaBeeldenVerwijderdService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.app.Application;
import ca.uhn.hl7v2.app.SimpleServer;
import ca.uhn.hl7v2.model.v24.message.ORM_O01;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.protocol.ApplicationWrapper;

@Service
@DependsOn("applicationContextProvider")
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class ScreenITHL7v2ServerServiceImpl implements ScreenITHL7v2ServerService
{

	@Autowired
	@Qualifier("hpvPort")
	private Integer incomingHpvPort;

	@Autowired
	@Qualifier("hl7IfobtPort")
	private Integer incomingIfobtPort;

	@Autowired
	@Qualifier("hl7ImsPort")
	private Integer incomingORMIMSPort;

	@Autowired
	@Qualifier("hl7IlmPort")
	private Integer incomingILMPort;

	@PostConstruct
	public void init()
	{
		createHpvServer();
		createIfobtServer();
		createMammaServers();
	}

	private void createMammaServers()
	{
		createIMSServer();
		createILMServer();
	}

	private void createIMSServer()
	{
		if (incomingORMIMSPort != null)
		{
			LOG.info("IMS ORM server wordt opgezet, op poort: {}", incomingORMIMSPort);
			Application handler = new MammaHL7v24Handler(ORM_O01.class, MammaBeeldenOntvangenService.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);
			createServer(wrapper, incomingORMIMSPort);
			LOG.info("IMS ORM server is aangemaakt.");
		}
		else
		{
			LOG.warn("Geen poortnummer gevonden voor de inkomende ORM berichten van het IMS.");
		}
	}

	private void createILMServer()
	{
		if (incomingILMPort != null)
		{
			LOG.info("IMS ILM ORM server wordt opgezet, op poort: {}", incomingILMPort);
			Application handler = new MammaHL7v24Handler(ORM_O01.class, MammaBeeldenVerwijderdService.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);
			createServer(wrapper, incomingILMPort);
			LOG.info("IMS ILM ORM server is aangemaakt.");
		}
		else
		{
			LOG.warn("Geen poortnummer gevonden voor de inkomende ORM ILM berichten van het IMS.");
		}
	}

	private void createHpvServer()
	{
		if (incomingHpvPort != null)
		{
			LOG.info("HPV HL7v251 server wordt opgezet, op poort: {}", incomingHpvPort);
			Application handler = new CervixHpvHL7v251Handler(OUL_R22.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);
			createServer(wrapper, incomingHpvPort);
			LOG.info("HPV HL7v251 server is aangemaakt.");
		}
		else
		{
			LOG.warn("Geen poortnummers in de configuratie gevonden voor HPV servers;");
		}
	}

	private void createIfobtServer()
	{
		if (incomingIfobtPort != null)
		{
			LOG.info("FIT HL7v251 server wordt opgezet, op poort: {}", incomingIfobtPort);
			Application handler = new ColonIFobtHL7v251Handler(OUL_R22.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);
			createServer(wrapper, incomingIfobtPort);
			LOG.info("FIT HL7v251 server is aangemaakt.");
		}
		else
		{
			LOG.warn("Geen poortnummers in de configuratie gevonden voor FIT servers;");
		}
	}

	private void createServer(ApplicationWrapper wrapper, int port)
	{
		SimpleServer server = new SimpleServer(port);
		server.registerApplication(wrapper);
		server.start();
		LOG.info("HL7v2 server is gestart op poort " + port);
	}
}
