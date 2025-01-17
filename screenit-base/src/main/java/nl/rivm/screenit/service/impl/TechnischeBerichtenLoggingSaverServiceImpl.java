package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.model.logging.TechnischeBerichtenLogRegel;
import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TechnischeBerichtenLoggingSaverServiceImpl implements TechnischeBerichtenLoggingSaverService
{
	private static final Logger LOGGER = LoggerFactory.getLogger(TechnischeBerichtenLoggingSaverServiceImpl.class);

	private static final ExecutorService EXECUTOR_SERVICE = Executors.newSingleThreadExecutor();

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private String applicationInstance;

	private class TechnischeLogEvent
	{

		private final LocalDateTime timestamp = LocalDateTime.now();

		private final boolean request;

		private String type;

		private String message;

		private long exchangeId;

		private String service;

		public TechnischeLogEvent(boolean request)
		{
			this.request = request;
		}
	}

	@Override
	public long logRequest(String type, String service, String message)
	{
		long exchangeId = createExchangeId();
		logRequest(type, exchangeId, service, message);
		return exchangeId;
	}

	@Override
	public void logRequest(String type, long exchangeId, String service, String message)
	{
		TechnischeLogEvent event = new TechnischeLogEvent(true);

		event.type = type;
		event.exchangeId = exchangeId;
		event.service = StringUtils.left(service, 255);
		event.message = message;

		logEvent(event);

	}

	@Override
	public void logResponse(String type, long exchangeId, String message)
	{
		TechnischeLogEvent event = new TechnischeLogEvent(false);

		event.type = type;
		event.exchangeId = exchangeId;
		event.message = message;
		logEvent(event);
	}

	private void logEvent(TechnischeLogEvent logEvent)
	{
		String logVerwijzing = logEvent.type + ": Vind %s bericht met 'select * from gedeeld.technische_berichten_log_regel where id = " + logEvent.exchangeId + "';";
		if (logEvent.request)
		{
			LOGGER.info(String.format(logVerwijzing, "request"));
		}
		else
		{
			LOGGER.info(String.format(logVerwijzing, "response"));
		}
		EXECUTOR_SERVICE.submit(new LoggingSaverThread(logEvent));
	}

	private class LoggingSaverThread extends OpenHibernate5SessionInThread
	{

		private TechnischeLogEvent logEvent;

		public LoggingSaverThread(TechnischeLogEvent logEvent)
		{
			super(true);
			this.logEvent = logEvent;
		}

		@Override
		protected void runInternal()
		{
			if (logEvent.request)
			{

				TechnischeBerichtenLogRegel logRegel = new TechnischeBerichtenLogRegel();
				logRegel.setId(logEvent.exchangeId);
				logRegel.setService(logEvent.service);
				logRegel.setApplicationInstance(applicationInstance);
				logRegel.setRequest(logEvent.message);
				logRegel.setRequestMoment(DateUtil.toUtilDate(logEvent.timestamp));
				hibernateService.save(logRegel);
			}
			else
			{

				TechnischeBerichtenLogRegel logRegel = hibernateService.get(TechnischeBerichtenLogRegel.class, logEvent.exchangeId);
				logRegel.setResponse(logEvent.message);
				logRegel.setResponseMoment(DateUtil.toUtilDate(logEvent.timestamp));
				hibernateService.saveOrUpdate(logRegel);
			}
		}

	}

	@Override
	public long createExchangeId()
	{
		Session session = hibernateService.getHibernateSession();
		return session.doReturningWork(new SequenceGenerator(DatabaseSequence.EXCHANGE_ID, session.getSessionFactory()));
	}
}
