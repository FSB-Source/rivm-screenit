package nl.rivm.screenit.main.rest.zorgid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.ZorgIdSessieService;
import nl.topicuszorg.zorgid.webservice.OpenCancelledEvent;
import nl.topicuszorg.zorgid.webservice.SessionClosedEvent;
import nl.topicuszorg.zorgid.webservice.SessionInitializedEvent;
import nl.topicuszorg.zorgid.webservice.SessionOpenedEvent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/zorgid")
public class ZorgidWebservice
{
	@Autowired
	private ZorgIdSessieService zorgIdSessieService;

	@RequestMapping(value = "/initialized", method = RequestMethod.POST)
	public ResponseEntity<?> initialized(@RequestBody SessionInitializedEvent sessionInitializedEvent)
	{
		zorgIdSessieService.onSessionInitialized(sessionInitializedEvent.getUuid(), sessionInitializedEvent.getNonce());
		return ResponseEntity.ok().build();
	}

	@RequestMapping(value = "/opened", method = RequestMethod.POST)
	public ResponseEntity<?> opened(@RequestBody SessionOpenedEvent sessionOpenedEvent)
	{
		zorgIdSessieService.onSessionOpened(sessionOpenedEvent.getUuid(), sessionOpenedEvent.getCertificate());
		return ResponseEntity.ok().build();
	}

	@RequestMapping(value = "/opencancelled", method = RequestMethod.POST)
	public ResponseEntity<?> opencancelled(@RequestBody OpenCancelledEvent openCancelledEvent)
	{
		zorgIdSessieService.onSessionOpenCancelled(openCancelledEvent.getUuid(), openCancelledEvent.getReason());
		return ResponseEntity.ok().build();
	}

	@RequestMapping(value = "/closed", method = RequestMethod.POST)
	public ResponseEntity<?> closed(@RequestBody SessionClosedEvent sessionClosedEvent)
	{
		zorgIdSessieService.onSessionClosed(sessionClosedEvent.getUuid(), sessionClosedEvent.getReason());
		return ResponseEntity.ok().build();

	}
}
