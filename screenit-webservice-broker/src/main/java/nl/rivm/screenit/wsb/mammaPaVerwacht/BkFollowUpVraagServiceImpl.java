
package nl.rivm.screenit.wsb.mammaPaVerwacht;

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

import javax.jws.WebMethod;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import nl.rivm.screenit.service.mamma.MammaBasePaVerslagService;
import nl.rivm.screenit.ws.bk.paverwacht.BkFollowUpVraagService;
import nl.rivm.screenit.ws.bk.paverwacht.VraagException_Exception;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service("BkFollowUpVraagService_PortType")
@Transactional(propagation = Propagation.SUPPORTS)
@WebService(targetNamespace = "http://screenit.rivm.nl/", name = "BkFollowUpVraagService")
public class BkFollowUpVraagServiceImpl implements BkFollowUpVraagService
{

	private static final Logger LOG = LoggerFactory.getLogger(BkFollowUpVraagServiceImpl.class);

	@Autowired
	private MammaBasePaVerslagService basePaVerslagService;

	@WebMethod
	@RequestWrapper(localName = "verwachtGegevensVoor", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.ws.bk.paverwacht.VerwachtGegevensVoor")
	@ResponseWrapper(
		localName = "verwachtGegevensVoorResponse",
		targetNamespace = "http://screenit.rivm.nl/",
		className = "nl.rivm.screenit.ws.bk.paverwacht.VerwachtGegevensVoorResponse")
	@WebResult(name = "verwachtGegevens", targetNamespace = "")
	@Override
	public boolean verwachtGegevensVoor(String bsn) throws VraagException_Exception
	{
		LOG.info("Vraag voor client " + bsn);
		return basePaVerslagService.verwachtGegevensVoor(bsn);
	}
}
