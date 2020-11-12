package nl.rivm.screenit.wsb.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.handler.CervixHpvHL7v251Handler;
import nl.rivm.screenit.handler.ColonIFobtHL7v251Handler;
import nl.rivm.screenit.handler.MammaHL7v24Handler;
import nl.rivm.screenit.wsb.service.ScreenITHL7v2ServerService;
import nl.rivm.screenit.wsb.service.mamma.MammaBeeldenOntvangenService;
import nl.rivm.screenit.wsb.service.mamma.MammaBeeldenVerwijderdService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.app.Application;
import ca.uhn.hl7v2.app.SimpleServer;
import ca.uhn.hl7v2.model.v24.message.ORM_O01;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.protocol.ApplicationWrapper;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ScreenITHL7v2ServerServiceImpl implements ScreenITHL7v2ServerService
{
	private static final Logger LOG = LoggerFactory.getLogger(ScreenITHL7v2ServerServiceImpl.class);

	@Autowired
	@Qualifier("hpvPorts")
	private String incomingHpvPorts;

	@Autowired
	@Qualifier("ifobtPorts")
	private String incomingIfobtPorts;

	@Autowired
	@Qualifier("hl7ImsPort")
	private Integer incomingORMIMSPoort;

	@Autowired
	@Qualifier("hl7IlmPort")
	private Integer incomingILMPoort;

	@PostConstruct
	public void init()
	{
		createHpvServers();
		createIfobtServers();
		createMammaServers();
	}

	private void createMammaServers()
	{
		createIMSServer();
		createILMServer();
	}

	private void createIMSServer()
	{
		if (incomingORMIMSPoort != null)
		{
			LOG.info("IMS ORM server wordt opgezet, op poort: " + incomingORMIMSPoort);
			Application handler = new MammaHL7v24Handler(ORM_O01.class, MammaBeeldenOntvangenService.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);
			createServer(wrapper, incomingORMIMSPoort);
			LOG.info("IMS ORM server is aangemaakt.");
		}
		else
		{
			LOG.warn("Geen poortnummer gevonden voor de inkomende ORM berichten van het IMS.");
		}
	}

	private void createILMServer()
	{
		if (incomingILMPoort != null)
		{
			LOG.info("IMS ILM ORM server wordt opgezet, op poort: " + incomingILMPoort);
			Application handler = new MammaHL7v24Handler(ORM_O01.class, MammaBeeldenVerwijderdService.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);
			createServer(wrapper, incomingILMPoort);
			LOG.info("IMS ILM ORM server is aangemaakt.");
		}
		else
		{
			LOG.warn("Geen poortnummer gevonden voor de inkomende ORM ILM berichten van het IMS.");
		}
	}

	private void createHpvServers()
	{
		if (incomingHpvPorts != null && !StringUtils.isBlank(incomingHpvPorts))
		{
			LOG.info("HPV HL7v251 servers worden opgezet!");

			Application handler = new CervixHpvHL7v251Handler(OUL_R22.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);

			for (String poort : incomingHpvPorts.split(","))
			{
				createServer(wrapper, Integer.parseInt(poort.trim()));
			}
			LOG.info("Alle HPV HL7v251 servers zijn aangemaakt");
			return;
		}
		LOG.warn("Geen poortnummers in de configuratie gevonden voor HPV servers;");
	}

	private void createIfobtServers()
	{
		if (incomingIfobtPorts != null && !StringUtils.isBlank(incomingIfobtPorts))
		{
			LOG.info("FIT HL7v251 servers worden opgezet!");

			Application handler = new ColonIFobtHL7v251Handler(OUL_R22.class);
			ApplicationWrapper wrapper = new ApplicationWrapper(handler);

			for (String poort : incomingIfobtPorts.split(","))
			{
				createServer(wrapper, Integer.parseInt(poort.trim()));
			}
			LOG.info("Alle FIT HL7v251 servers zijn aangemaakt");
			return;
		}
		LOG.warn("Geen poortnummers in de configuratie gevonden voor FIT servers;");
	}

	private void createServer(ApplicationWrapper wrapper, int port)
	{
		SimpleServer server = new SimpleServer(port);
		server.registerApplication(wrapper);
		server.start();
		LOG.info("HL7v2 server is gestart op poort " + port);
	}
}
