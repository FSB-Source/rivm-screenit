package nl.rivm.screenit.wsb.organisatieboom;

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

import java.util.ArrayList;
import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.ResponseWrapper;

import nl.rivm.screenit.model.OrganisatieBoomWrapper;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.ws.organisatieboom.ColonisOrganisatie;
import nl.rivm.screenit.ws.organisatieboom.OrganisatieBoomExceptionException;
import nl.rivm.screenit.ws.organisatieboom.OrganisatieBoomService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service("OrganisatieBoomService_PortType")
@Transactional(propagation = Propagation.SUPPORTS)
@WebService(targetNamespace = "http://screenit.rivm.nl/", name = "OrganisatieBoomService")
public class OrganisatieBoomServiceImpl implements OrganisatieBoomService
{
	
	private static final Logger LOG = LoggerFactory.getLogger(OrganisatieBoomServiceImpl.class);

	@Autowired
	private InstellingService instellingService;

	@Override
	@WebResult(name = "organisatielijst", targetNamespace = "")
	@WebMethod
	@ResponseWrapper(localName = "organisatieBoomResponse", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.ws.organisatieboom.OrganisatieBoomResponse")
	@Transactional(propagation = Propagation.SUPPORTS)
	public List<ColonisOrganisatie> getOrganisatieBoom() throws OrganisatieBoomExceptionException
	{
		LOG.info("Verzoek voor organisatieboom ontvangen.");
		List<ColonisOrganisatie> lijst = new ArrayList<ColonisOrganisatie>();
		for (OrganisatieBoomWrapper wrapper : instellingService.getCompleteOrganisatieBoom())
		{
			ColonisOrganisatie org = new ColonisOrganisatie();
			org.setNaam(wrapper.getNaam());
			org.setOrganisatieId(wrapper.getOrganisatieId());
			org.setParentId(wrapper.getParentId());
			org.setType(wrapper.getType().name());
			lijst.add(org);
		}
		LOG.info("Klaar met verwerken verzoek voor organisatieboom.");
		return lijst;
	}
}
