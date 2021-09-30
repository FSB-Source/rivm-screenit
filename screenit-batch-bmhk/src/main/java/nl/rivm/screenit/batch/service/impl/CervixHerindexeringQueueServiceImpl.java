package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.batch.service.CervixHerindexeerVerrichtingenService;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHerindexatieDto;
import nl.rivm.screenit.service.MessageService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.fasterxml.jackson.databind.ObjectMapper;

@Service
public class CervixHerindexeringQueueServiceImpl
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixHerindexeringQueueServiceImpl.class);

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private CervixHerindexeerVerrichtingenService herindexatieService;

	@Autowired
	private MessageService messageService;

	private boolean bindSessionOnThread = false;

	private int heartbeatCounter = 0;

	private final ObjectMapper objectMapper = new ObjectMapper();

	@PostConstruct
	public void init()
	{

		LOG.info("Starting message queue verwerker voor herindexatie met params: BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE="
			+ Integer.getInteger("BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE", 500) + " en BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS= "
			+ Integer.getInteger("BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS", 2000));

		ExecutorService executorService = Executors.newSingleThreadExecutor();
		executorService.submit(() -> {
			AtomicInteger totaalAantalVerrichtingen = new AtomicInteger(0);
			CervixHerindexatieDto herindexatieDto = null;
			try
			{
				while (true)
				{
					bindSession();
					Message message = messageService.getOldestMessage(MessageType.HERINDEXATIE);
					if (message != null)
					{
						herindexatieDto = messageService.getContent(message);
						totaalAantalVerrichtingen.set(0);
						verwerkQueueMessage(herindexatieDto, totaalAantalVerrichtingen);
						messageService.dequeueMessage(message);
					}
					unbindSessionFactory();
					wachtOpVolgendeOpdracht();
				}
			}
			catch (Exception e)
			{
				bindSession();
				herindexatieService.logFout(herindexatieDto, totaalAantalVerrichtingen.get(), e);
				return;
			}
			finally
			{
				unbindSessionFactory();
			}
		});

	}

	private void verwerkQueueMessage(CervixHerindexatieDto herindexatieDto, AtomicInteger totaalAantalVerrichtingen) throws InterruptedException
	{
		herindexatieService.logStart(herindexatieDto);
		int aantalVerrichtingen;
		do
		{
			aantalVerrichtingen = herindexatieService.verrichtingenHerindexeren(herindexatieDto);
			totaalAantalVerrichtingen.addAndGet(aantalVerrichtingen);
			LOG.info(aantalVerrichtingen + " (verdere) verrichtingen zijn aangepast.");
			if (herindexatieDto.isHuisartsTarief() && aantalVerrichtingen > 0)
			{

				Thread.sleep(Integer.getInteger("BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS", 200));
			}
		}
		while (aantalVerrichtingen > 0);

		herindexatieService.logEinde(herindexatieDto, totaalAantalVerrichtingen.get());

	}
	private void wachtOpVolgendeOpdracht()
	{
		try
		{
			Thread.sleep(3000L);
		}
		catch (InterruptedException e)
		{
		}
		if (heartbeatCounter++ % 20 == 0)
		{
			LOG.info("Heartbeat verwerkHerindexatieVerzoeken");
		}
	}

	private void bindSession()
	{
		if (!bindSessionOnThread)
		{
			Session session = sessionFactory.openSession();
			TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(session));
			bindSessionOnThread = true;
		}
	}

	private void unbindSessionFactory()
	{
		if (bindSessionOnThread)
		{
			SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.unbindResource(this.sessionFactory);
			SessionFactoryUtils.closeSession(sessionHolder.getSession());
			bindSessionOnThread = false;
		}
	}
}
