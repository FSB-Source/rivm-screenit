package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.concurrent.LinkedBlockingDeque;

import nl.rivm.screenit.mamma.se.proxy.model.CacheProxyActie;
import nl.rivm.screenit.mamma.se.proxy.model.OphaalRequest;
import nl.rivm.screenit.mamma.se.proxy.model.RequestTypeCentraal;
import nl.rivm.screenit.mamma.se.proxy.model.SeStatusDto;
import nl.rivm.screenit.mamma.se.proxy.model.WebsocketBerichtType;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.WebSocketProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.http.RequestEntity;
import org.springframework.stereotype.Service;

@Service
public class AchtergrondRequestServiceImpl implements AchtergrondRequestService
{
	private static final Logger LOG = LoggerFactory.getLogger(AchtergrondRequestServiceImpl.class);

	private final Object createOphaalThreadLock = new Object(); 

	private final LinkedBlockingDeque<OphaalRequest> opTeHalenRequestsQueue = new LinkedBlockingDeque<>();

	private final Object opTeHalenRequestsToevoegenLock = new Object(); 

	private OphaalThread ophaalThread;

	@Autowired
	private SeStatusService statusService;

	@Autowired
	private SeDaglijstService daglijstService;

	@Autowired
	private WebSocketProxyService webSocketProxyService;

	@Autowired
	private ProxyService proxyService;

	@Override
	public void queueDaglijstRequest(LocalDate opTeHalenDag)
	{
		queueRequest(new OphaalRequest(RequestTypeCentraal.GET_DAGLIJST, opTeHalenDag, () -> haalDaglijstEnBroadcast(opTeHalenDag)));
	}

	private void haalDaglijstEnBroadcast(LocalDate opTeHalenDag)
	{
		String daglijstResponse = daglijstService.getDaglijstGeforceerd(opTeHalenDag);
		if (daglijstResponse != null && DateUtil.isVandaag(opTeHalenDag))
		{
			webSocketProxyService.broadcast(WebsocketBerichtType.DAGLIJST_UPDATE.name());
		}
	}

	@Override
	public void verwijderAlleOpTeHalenDaglijsten()
	{
		opTeHalenRequestsQueue.removeIf(r -> r.getRequestType() == RequestTypeCentraal.GET_DAGLIJST);
	}

	@Override
	public void queueStatusPostenRequest(SeStatusDto statusDto)
	{
		queueRequest(new OphaalRequest(RequestTypeCentraal.POST_STATUS, () -> {
			RequestEntity.BodyBuilder requestBuilder = proxyService.getProxyRequestEntity("/status", HttpMethod.POST);
			proxyService.sendUncheckedProxyRequest(requestBuilder.body(statusDto), SeStatusDto.class);
		}));
	}

	@Override
	public void queueStamdataVerversRequests(CacheProxyActie cacheActie)
	{
		queueRequest(stamDataOphaalRequest(RequestTypeCentraal.GET_MAMMOGRAFEN, cacheActie));
		queueRequest(stamDataOphaalRequest(RequestTypeCentraal.GET_ZORGINSTELLINGEN, cacheActie));
		queueRequest(stamDataOphaalRequest(RequestTypeCentraal.GET_HUISARTSEN, cacheActie));
	}

	private OphaalRequest stamDataOphaalRequest(RequestTypeCentraal requestType, CacheProxyActie cacheActie)
	{
		return new OphaalRequest(requestType, () -> proxyService.getRequest(requestType, cacheActie));
	}

	@Override
	public void queueVerversGeenScreeningBlokken(LocalDate opTeHalenDag, CacheProxyActie cacheActie)
	{
		queueRequest(new OphaalRequest(RequestTypeCentraal.GET_PLANNING, opTeHalenDag, () -> proxyService.getPlanning(opTeHalenDag, cacheActie)));
	}

	private void queueRequest(OphaalRequest ophaalRequest)
	{
		synchronized (opTeHalenRequestsToevoegenLock)
		{
			if (!opTeHalenRequestsQueue.contains(ophaalRequest))
			{
				opTeHalenRequestsQueue.add(ophaalRequest);
			}
		}
	}

	@Override
	public void ensureRunning()
	{
		synchronized (createOphaalThreadLock)
		{
			if (ophaalThread == null || !ophaalThread.isAlive())
			{
				ophaalThread = new OphaalThread();
				ophaalThread.start();
			}
		}
	}

	private class OphaalThread extends Thread
	{
		@Override
		public void run()
		{
			while (magDataOphalen())
			{
				try
				{
					OphaalRequest ophaalRequest = opTeHalenRequestsQueue.take();
					LOG.info("Start ophaalactie voor {} {}", ophaalRequest.getRequestType(), ophaalRequest.getDatum() != null ? "met datum: " + ophaalRequest.getDatum() : "");
					ophaalRequest.run();
				}
				catch (Exception ex)
				{
					throw new IllegalStateException("Onbekende fout in ophalen achtergrondrequest: " + ex.getMessage());
				}
			}
		}
	}

	private boolean magDataOphalen()
	{
		return statusService.isOnline() && !opTeHalenRequestsQueue.isEmpty();
	}
}
