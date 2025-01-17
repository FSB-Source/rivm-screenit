package nl.rivm.screenit.mamma.se.websocket.socket;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.io.IOException;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.websocket.WebsocketBerichtType;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketClose;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketError;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketMessage;
import org.eclipse.jetty.websocket.api.annotations.WebSocket;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;

@WebSocket
@Service
public class SeProxyWebsocket implements ApplicationContextAware
{
	private static final Logger LOG = LoggerFactory.getLogger(SeProxyWebsocket.class);

	private static final String SE_CODE_ONBEKEND = "Onbekend";

	private static ApplicationContext applicationContext;

	private static final Map<String, Session> proxySessionMap = new ConcurrentHashMap<>();

	private MammaScreeningsEenheidStatusService statusService;

	private SELogService logService;

	private ICurrentDateSupplier dateSupplier;

	private Boolean testModus;

	@OnWebSocketMessage
	public void receiveMessage(Session session, String webSocketMessage)
	{
		try
		{
			synchronized (proxySessionMap)
			{
				if (webSocketMessage.equals(WebsocketBerichtType.PING.name()))
				{
					String se = getSeCodeVanSession(session);
					if (SE_CODE_ONBEKEND.equals(se))
					{
						session.close();
						session.disconnect();
						LOG.info("PING ontvangen van SE met code die niet aanwezig is in de sessionMap, session gesloten. session: {}", session.hashCode());
					}
					else
					{
						LOG.debug("PING ontvangen van SE met code: {}; session: {}", se, session.hashCode());
						stuurCommandoNaarSe(session, WebsocketBerichtType.PONG);
					}
				}
				else if (webSocketMessage.startsWith(WebsocketBerichtType.REGISTREER_SE.name()))
				{
					String seCode = webSocketMessage.replace(WebsocketBerichtType.REGISTREER_SE.name(), "");
					Session oldSession = proxySessionMap.get(seCode);
					if (oldSession != null)
					{
						oldSession.close();
						oldSession.disconnect();
					}
					proxySessionMap.put(seCode, session);
					logVerbindingStatus(LogGebeurtenis.MAMMA_SE_WEBSOCKET_VERBINDING_GEMAAKT, seCode);
				}
			}
		}
		catch (Exception e)
		{
			LOG.info("Kon request van SE '{}' niet beantwoorden. session: {}", getSeCodeVanSession(session), session.hashCode());
		}
	}

	public void sendDaglijstUpdate(String seCodeEnDatum)
	{
		String seCode = seCodeEnDatum.split(":")[0];

		Session proxySession = proxySessionMap.get(seCode);
		if (proxySession == null)
		{
			LOG.info("Kon geen websocket vinden voor SE met code '{}', update commando wordt niet verzonden.", seCode);
		}
		else
		{
			new Timer().schedule(
				new TimerTask()
				{
					@Override
					public void run()
					{
						try
						{
							LOG.info("Daglijst update commando wordt verzonden naar SE met code en datum {}", seCodeEnDatum);
							stuurCommandoNaarSe(proxySession, WebsocketBerichtType.DAGLIJST_UPDATE + ":" + seCodeEnDatum);
						}
						catch (Exception ex)
						{
							LOG.error("Daglijst update {} versturen mislukt: ", seCodeEnDatum, ex);
						}
					}
				},
				5000); 
		}
	}

	private void stuurCommandoNaarSe(Session session, WebsocketBerichtType berichtType) throws Exception
	{
		stuurCommandoNaarSe(session, berichtType.name());
	}

	private void stuurCommandoNaarSe(Session session, String commando) throws Exception
	{

		Future<Void> sendStringFuture = session.getRemote().sendStringByFuture(commando);
		sendStringFuture.get(10, TimeUnit.SECONDS);
	}

	public void sendTijdUpdateNaarIedereSe(Duration offset)
	{
		sendTestCommandoNaarSEs(WebsocketBerichtType.TIJD_UPDATE + ":" + offset.toString());
	}

	public void sendDbCleanupNaarIedereSe()
	{
		sendTestCommandoNaarSEs(WebsocketBerichtType.DB_CLEANUP);
	}

	public void sendVerstuurStatusVerzoekNaarIedereSe()
	{
		sendCommandoNaarSEs(WebsocketBerichtType.VERSTUUR_STATUS);
	}

	private void sendTestCommandoNaarSEs(WebsocketBerichtType berichtType)
	{
		sendTestCommandoNaarSEs(berichtType.name());
	}

	private void sendTestCommandoNaarSEs(String commando)
	{
		if (inTestModus())
		{
			sendCommandoNaarSEs(commando);
		}
	}

	private void sendCommandoNaarSEs(WebsocketBerichtType berichtType)
	{
		sendCommandoNaarSEs(berichtType.name());
	}

	private void sendCommandoNaarSEs(String commando)
	{
		new Timer().schedule(
			new TimerTask()
			{
				@Override
				public void run()
				{
					try
					{
						synchronized (proxySessionMap)
						{
							LOG.info("Commando '{}' wordt verzonden naar de volgende SE's: {}", commando, proxySessionMap.keySet());
							for (String seCode : proxySessionMap.keySet())
							{
								Session proxySession = proxySessionMap.get(seCode);
								if (proxySession == null)
								{
									LOG.info("Kon geen websocket vinden voor SE met code '{}', commando '{}' wordt niet verzonden.", seCode, commando);
								}
								else
								{
									LOG.info("Commando '{}' wordt verzonden naar {}", commando, seCode);
									stuurCommandoNaarSe(proxySession, commando);
								}
							}
						}
					}
					catch (Exception ex)
					{
						LOG.error("Commando {} naar SE's sturen mislukt: ", commando, ex);
					}

				}
			},
			1500); 
	}

	private boolean inTestModus()
	{
		if (testModus == null)
		{
			testModus = applicationContext.getBean("testModus", Boolean.class);
		}
		return Boolean.TRUE.equals(testModus);
	}

	@OnWebSocketError
	public void handleTransportError(Session session, Throwable error)
	{
		synchronized (proxySessionMap)
		{
			String seCode = getSeCodeVanSession(session);
			LOG.warn("Websocket verbinding SE '{}' verbroken door '{}'; session: {}" + seCode, error.getMessage(), session.hashCode());
			session.close();
		}
	}

	@OnWebSocketClose
	public void afterConnectionClosed(Session session, int status, String message) throws IOException
	{
		synchronized (proxySessionMap)
		{
			String seCode = getSeCodeVanSession(session);
			LOG.warn("afterConnectionClosed: status={}, message={}, seCode={}, session={}", status, message, seCode, session.hashCode());
			if (!SE_CODE_ONBEKEND.equals(seCode))
			{
				logVerbindingStatus(LogGebeurtenis.MAMMA_SE_WEBSOCKET_VERBINDING_VERLOREN, seCode);
			}
			proxySessionMap.remove(seCode);
			session.close();
			session.disconnect();
		}
	}

	private String getSeCodeVanSession(Session session)
	{
		String seCode = SE_CODE_ONBEKEND;
		for (Map.Entry<String, Session> entry : proxySessionMap.entrySet())
		{
			if (Objects.equals(session, entry.getValue()))
			{
				seCode = entry.getKey();
			}
		}
		return seCode;
	}

	private void logVerbindingStatus(LogGebeurtenis logGebeurtenis, String seCode)
	{
		if (logService == null)
		{
			logService = applicationContext.getBean(SELogService.class);
		}
		if (dateSupplier == null)
		{
			dateSupplier = applicationContext.getBean(ICurrentDateSupplier.class);
		}
		if (statusService == null)
		{
			statusService = applicationContext.getBean(MammaScreeningsEenheidStatusService.class);
		}
		logService.logInfo(logGebeurtenis, null, seCode, dateSupplier.getLocalDateTime(), null);
		if (LogGebeurtenis.MAMMA_SE_WEBSOCKET_VERBINDING_GEMAAKT.equals(logGebeurtenis))
		{
			statusService.slaVerbindingStatusOp(seCode, true);
		}
		else if (LogGebeurtenis.MAMMA_SE_WEBSOCKET_VERBINDING_VERLOREN.equals(logGebeurtenis))
		{
			statusService.slaVerbindingStatusOp(seCode, false);
		}
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException
	{
		SeProxyWebsocket.applicationContext = applicationContext;
	}
}
