package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.net.ConnectException;
import java.net.URI;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

import javax.websocket.ClientEndpoint;
import javax.websocket.ContainerProvider;
import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.Session;
import javax.websocket.WebSocketContainer;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.dao.PersistableTransactionDao;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.model.WebsocketBerichtType;
import nl.rivm.screenit.mamma.se.proxy.services.CleanUpService;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.services.WebSocketProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;

@Service
@ClientEndpoint
@Slf4j
public class SeRestSocketServiceImpl implements SeRestSocketService, ApplicationContextAware
{

	private static final int RETRY_TIMEOUT = 5000;

	private static final int PING_INTERVAL_DEFAULT_MS = 2500;

	private int pingInterval = PING_INTERVAL_DEFAULT_MS;

	private static final int PONG_TIMEOUT_DEFAULT_MS = 5000;

	private int pongTimeout = PONG_TIMEOUT_DEFAULT_MS;

	private static final int MINIMALE_RETRY_TIMEOUT_IN_SECONDEN = 30;

	private static final int MAXIMALE_RETRY_TIMEOUT_IN_SECONDEN = 5 * 60;

	private RetryWebsocketVerbindingTask retryTask;

	private PingTask pingTask;

	private Timer timer;

	@Value("${SE_REST_URL}")
	private String seRestUrl;

	private PersistableTransactionDao transactionDao;

	private WebSocketProxyService webSocketProxyService;

	private SeStatusService statusService;

	private SeDaglijstService daglijstService;

	private MammaScreeningsEenheidStatusService screeningsEenheidStatusService;

	private CleanUpService cleanUpService;

	private ProxyService proxyService;

	@Autowired
	private ConfiguratieService configuratieService;

	private TransactionQueueService transactionQueueService;

	private static ApplicationContext applicationContext;

	private static volatile LocalDateTime laatstePong;

	private static volatile Session socketSession; 

	private static LocalDateTime minimaleRetryMoment = LocalDateTime.now();

	@Override
	public void initRestSocketService()
	{
		retrySocketVerbinding();
	}

	private void initSocket(String seCode)
	{
		if (magSocketInitieren())
		{
			try
			{
				verversPingEnPongConfig();
				laatstePong = null;
				String seRestEndpoint = seRestUrl.replace("http", "ws") + "/ws/proxySocket";
				LOG.info("Maak verbinding met: " + seRestEndpoint);
				WebSocketContainer container = ContainerProvider.getWebSocketContainer();
				if (!getStatusService().isTestOnline() && SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
				{
					throw new ConnectException("Test situatie in geforceerd offline. Websocket verbinding niet toegestaan.");
				}
				socketSession = container.connectToServer(SeRestSocketServiceImpl.class, URI.create(seRestEndpoint));
				LOG.info("Websocket verbonden, registreer SE code: {}", seCode);
				socketSession.getBasicRemote().sendText(WebsocketBerichtType.REGISTREER_SE.name() + seCode);
				LOG.info("SE geregistreerd, start ping task");
				scheduleNewPingTask();
			}
			catch (Exception e)
			{
				setSeVerbindingStatus(false);
				LOG.error("Fout bij verbinden met SE REST door {}", e.getMessage(), e);
			}
		}
	}

	private void scheduleNewPingTask()
	{
		getTimer().schedule(getPingTask(), 0, pingInterval);
	}

	@Override
	public void verversPingEnPongConfig()
	{
		var intervalUitConfig = configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_PING_INTERVAL);
		if (intervalUitConfig > 0)
		{
			var nieuwInterval = pingInterval != intervalUitConfig;
			if (nieuwInterval)
			{
				pingInterval = intervalUitConfig;
				scheduleNewPingTask();
			}
		}
		var timeoutUitConfig = configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_PONG_TIMEOUT);
		if (timeoutUitConfig > 0)
		{
			var nieuweTimeout = pongTimeout != timeoutUitConfig;
			if (nieuweTimeout)
			{
				pongTimeout = timeoutUitConfig;
			}
		}
	}

	private boolean magSocketInitieren()
	{
		return !webSocketOpen() && statusService.getSeCode() != null && minimaleRetryMoment.compareTo(LocalDateTime.now()) <= 0;
	}

	private boolean webSocketOpen()
	{
		return socketSession != null && socketSession.isOpen();
	}

	private String socketGeslotenStatusLogTekst()
	{
		if (socketSession == null)
		{
			return "socketSession is null";
		}
		else if (!socketSession.isOpen())
		{
			return "socketSession niet open";
		}
		return "overige reden";
	}

	@OnMessage
	public void onMessage(String message)
	{
		String messageSoort = message.split(":")[0];
		switch (WebsocketBerichtType.valueOf(messageSoort))
		{
		case PONG:
			if (webSocketOpen())
			{
				laatstePong = LocalDateTime.now();
				LOG.trace("Receive pong: was online: {}", getStatusService().isOnline());
				setSeVerbindingStatus(true);
			}
			else
			{
				LOG.info("Pong van centraal genegeerd omdat websocket gesloten is ({}). onlinestatus: {}", socketGeslotenStatusLogTekst(), getStatusService().isOnline());
			}
			break;
		case DAGLIJST_UPDATE:
			LOG.info("Update daglijst verzoek ontvangen van SE-REST-BK");
			String[] params = message.split(":");
			LocalDate datum = getDaglijstUpdateDatum(params);
			getDaglijstService().queueDaglijstOphalenVanDag(datum);
			break;
		case TIJD_UPDATE:
			if (SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
			{
				String offset = message.split(":")[1];
				LOG.info("Update tijd ontvangen van SE-REST-BK, offset: " + offset);
				DateUtil.setOffset(Duration.parse(offset));
				getWebSocketProxyService().broadCastTijdUpdateNaarWerkstations("SE-REST-BK");
			}
			break;
		case DB_CLEANUP:
			if (SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
			{
				LOG.info("Cleanup DB ontvangen van SE-REST-BK");
				getPersistableTransactionDao().clearDb();
				getProxyService().clearTestCache();
				getCleanupService().startOfDayCleanup();
			}
			break;
		case VERSTUUR_STATUS:
			LOG.info("Verstuur status ontvangen van SE-REST-BK");
			getScreeningsEenheidStatusService().maakStatusEnQueueRequestNaarCentraal();
			break;
		default:
			LOG.warn("Onbekend bericht ontvangen van Centraal: " + message);
		}
	}

	private LocalDate getDaglijstUpdateDatum(String[] params)
	{
		LocalDate datum;
		if (params.length >= 3)
		{
			String paramDatum = params[2];
			try
			{
				datum = LocalDate.from(DateTimeFormatter.ISO_DATE.parse(paramDatum));
			}
			catch (DateTimeParseException e)
			{
				datum = DateUtil.getCurrentDateTime().toLocalDate();
				LOG.warn("Datum in DAGLIJST_UPDATE call kon niet geparsed worden. Gebruik datum van vandaag.", e);
			}
		}
		else
		{
			datum = DateUtil.getCurrentDateTime().toLocalDate();
			LOG.warn("Datum mist in DAGLIJST_UPDATE call. Gebruik datum van vandaag.");
		}
		return datum;
	}

	@OnClose
	public void onClose()
	{
		LOG.info("onClose: Verbinding met SE-REST-BK gesloten");
		setSeVerbindingStatus(false);
	}

	@OnError
	public void onError(Throwable error)
	{
		closeSocket();
		LOG.error("Socket error: " + error.getMessage());
	}

	@Override
	public void closeSocket()
	{
		try
		{
			if (socketSession != null)
			{
				socketSession.close();
			}
			setSeVerbindingStatus(false); 
		}
		catch (IOException e)
		{

		}
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException
	{
		SeRestSocketServiceImpl.applicationContext = applicationContext;
	}

	private class PingTask extends TimerTask
	{
		@Override
		public void run()
		{
			try
			{
				if (webSocketOpen() && isPongTimeout())
				{
					LOG.info("PingTask.run PONG timeout. Laatste PONG is langer dan 2 ping intervallen geleden.");
					closeSocket();
					this.cancel();
					retrySocketVerbinding();
				}
				else if (!webSocketOpen())
				{
					LOG.info("PingTask.run.else: {}", socketGeslotenStatusLogTekst());
					setSeVerbindingStatus(false);
					this.cancel();
					retrySocketVerbinding();
				}
				else
				{
					LOG.trace("PingTask.send PING. Online: {}", getStatusService().isOnline());
					socketSession.getBasicRemote().sendText(WebsocketBerichtType.PING.name());
				}
			}
			catch (Exception e)
			{
				LOG.warn("PingTask.catch: Verbinding met centraal verbroken: " + e.getMessage());
				closeSocket();
				this.cancel();
				retrySocketVerbinding();
			}
		}

		private boolean isPongTimeout()
		{
			return laatstePong != null && !LocalDateTime.now().isBefore(laatstePong.plusNanos((pongTimeout * 1000000L)));
		}
	}

	@Override
	public void sluitSocketEnSetRandomRetry()
	{
		Random random = new Random();
		int randomDelay = random.nextInt(MAXIMALE_RETRY_TIMEOUT_IN_SECONDEN - MINIMALE_RETRY_TIMEOUT_IN_SECONDEN) + MINIMALE_RETRY_TIMEOUT_IN_SECONDEN;
		minimaleRetryMoment = LocalDateTime.now().plusSeconds(randomDelay);
		LOG.info("Socket wordt gesloten door een timeout. Volgende retry na: " + DateUtil.format(minimaleRetryMoment));
		closeSocket();
	}

	private class RetryWebsocketVerbindingTask extends TimerTask
	{
		@Override
		public void run()
		{
			if (!webSocketOpen())
			{
				LOG.info("Probeer (opnieuw) verbinding te maken met centraal.");
				initSocket(getStatusService().getSeCode());
			}
			else
			{
				this.cancel();
			}
		}
	}

	private void retrySocketVerbinding()
	{

		getTimer().schedule(getRetryTask(), 1000, RETRY_TIMEOUT);
	}

	@Override
	public void setSeVerbindingStatus(boolean nieuweIsOnline)
	{
		boolean huidigeIsOnline = getStatusService().isOnline();
		if (huidigeIsOnline != nieuweIsOnline)
		{
			getWebSocketProxyService().broadcast(nieuweIsOnline ? WebsocketBerichtType.ONLINE.name() : WebsocketBerichtType.OFFLINE.name());
		}

		getStatusService().setVerbindingStatus(nieuweIsOnline);

		if (!huidigeIsOnline && nieuweIsOnline)
		{
			getTransactionQueueService().ensureRunningQueueVerwerking();
			getCleanupService().updateCachesAtOnlineEvent();
		}
	}

	private TransactionQueueService getTransactionQueueService()
	{
		if (transactionQueueService == null)
		{
			transactionQueueService = applicationContext.getBean(TransactionQueueService.class);
		}
		return transactionQueueService;
	}

	private WebSocketProxyService getWebSocketProxyService()
	{
		if (webSocketProxyService == null)
		{
			webSocketProxyService = applicationContext.getBean(WebSocketProxyService.class);
		}
		return webSocketProxyService;
	}

	private PersistableTransactionDao getPersistableTransactionDao()
	{
		if (transactionDao == null)
		{
			transactionDao = applicationContext.getBean(PersistableTransactionDao.class);
		}
		return transactionDao;
	}

	private SeStatusService getStatusService()
	{
		if (statusService == null)
		{
			statusService = applicationContext.getBean(SeStatusService.class);
		}
		return statusService;
	}

	private SeDaglijstService getDaglijstService()
	{
		if (daglijstService == null)
		{
			daglijstService = applicationContext.getBean(SeDaglijstService.class);
		}
		return daglijstService;
	}

	private MammaScreeningsEenheidStatusService getScreeningsEenheidStatusService()
	{
		if (screeningsEenheidStatusService == null)
		{
			screeningsEenheidStatusService = applicationContext.getBean(MammaScreeningsEenheidStatusService.class);
		}
		return screeningsEenheidStatusService;
	}

	private CleanUpService getCleanupService()
	{
		if (cleanUpService == null)
		{
			cleanUpService = applicationContext.getBean(CleanUpService.class);
		}
		return cleanUpService;
	}

	private ProxyService getProxyService()
	{
		if (proxyService == null)
		{
			proxyService = applicationContext.getBean(ProxyService.class);
		}
		return proxyService;
	}

	private Timer getTimer()
	{
		if (timer == null)
		{
			timer = new Timer();
		}
		return timer;
	}

	private RetryWebsocketVerbindingTask getRetryTask()
	{
		if (retryTask != null)
		{
			retryTask.cancel();
		}
		retryTask = new RetryWebsocketVerbindingTask();
		return retryTask;
	}

	private PingTask getPingTask()
	{
		if (pingTask != null)
		{
			pingTask.cancel();
		}
		pingTask = new PingTask();
		LOG.info("New PingTask created");
		return pingTask;
	}

}
