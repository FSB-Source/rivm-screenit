package nl.rivm.screenit.mamma.se.websocket.socket;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.io.IOException;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;

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

	private static Map<String, Session> proxySessionMap = new ConcurrentHashMap<>();

	private static final String REGISTREER_SE = "REGISTREER_SE";

	private static final String PING = "PING";

	private static final String PONG = "PONG";

	@OnWebSocketMessage
	public void receiveMessage(Session session, String webSocketMessage)
	{
		try
		{
			if (webSocketMessage.equals(PING))
			{
				String se = getSeCodeVanSession(session);
				if (se.equals(SE_CODE_ONBEKEND))
				{
					session.close();
					LOG.info("PING ontvangen van SE met code die niet aanwezig is in de sessionMap, session gesloten.");
				}
				else
				{
					LOG.debug("PING ontvangen van SE met code: " + se);
					session.getRemote().sendString(PONG);
				}
			}
			else if (webSocketMessage.startsWith(REGISTREER_SE))
			{
				String seCode = webSocketMessage.replace(REGISTREER_SE, "");
				proxySessionMap.put(seCode, session);
				logVerbindingGemaaktOfVerbroken(true, seCode);
			}
		}
		catch (IOException e)
		{
			LOG.info("Kon request van SE " + getSeCodeVanSession(session) + " niet beantwoorden.");
		}
	}

	public void sendDaglijstUpdate(String seCode)
	{
		Session proxySession = proxySessionMap.get(seCode);
		if (proxySession == null)
		{
			LOG.info("Kon geen websocket vinden voor SE met code: " + seCode + ", update commando wordt niet verzonden.");
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
							LOG.info("Daglijst update commando wordt verzonden naar SE met code " + seCode);
							proxySession.getRemote().sendString("DAGLIJST_UPDATE");
						}
						catch (IOException e)
						{
							LOG.error(e.getMessage());
						}
					}
				},
				5000); 
		}
	}

	public void sendTijdUpdateNaarIedereSe(Duration offset)
	{
		new Timer().schedule(
			new TimerTask()
			{
				@Override
				public void run()
				{
					try
					{
						LOG.info("Tijd update wordt verzonden naar de volgende SE's: " + proxySessionMap.keySet().toString());
						for (String seCode : proxySessionMap.keySet())
						{
							Session proxySession = proxySessionMap.get(seCode);
							if (proxySession == null)
							{
								LOG.info("Kon geen websocket vinden voor SE met code: " + seCode + ", tijd_update commando wordt niet verzonden.");
							}
							else
							{
								LOG.info("Tijd update commando wordt verzonden naar SE met code " + seCode);
								proxySession.getRemote().sendString("TIJD_UPDATE:" + offset.toString());
							}
						}
					}
					catch (IOException e)
					{
						LOG.error(e.getMessage());
					}
				}
			},
			1500); 
	}

	@OnWebSocketError
	public void handleTransportError(Session session, Throwable error)
	{
		String seCode = getSeCodeVanSession(session);
		LOG.info("Websocket verbinding SE: " + seCode + " verbroken door: " + error.getMessage());
		proxySessionMap.remove(seCode);
		session.close();
	}

	@OnWebSocketClose
	public void afterConnectionClosed(Session session, int status, String message)
	{
		String seCode = getSeCodeVanSession(session);
		logVerbindingGemaaktOfVerbroken(false, seCode);
		proxySessionMap.remove(seCode);
		session.close();
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

	private void logVerbindingGemaaktOfVerbroken(boolean verbindingGemaakt, String seCode)
	{
		if (verbindingGemaakt)
		{
			applicationContext.getBean(SELogService.class).logInfo(LogGebeurtenis.MAMMA_SE_WEBSOCKET_VERBINDING_GEMAAKT, null, seCode,
				applicationContext.getBean(ICurrentDateSupplier.class).getLocalDateTime(), seCode);
		}
		else
		{
			applicationContext.getBean(SELogService.class).logInfo(LogGebeurtenis.MAMMA_SE_WEBSOCKET_VERBINDING_VERLOREN, null, seCode,
				applicationContext.getBean(ICurrentDateSupplier.class).getLocalDateTime(), seCode);
		}
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException
	{
		SeProxyWebsocket.applicationContext = applicationContext;
	}
}
