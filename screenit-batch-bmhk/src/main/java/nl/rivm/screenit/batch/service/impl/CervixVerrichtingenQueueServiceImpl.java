package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.CervixHerindexeerVerrichtingenService;
import nl.rivm.screenit.batch.service.CervixVerwijderSepaDataService;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHerindexatieDto;
import nl.rivm.screenit.model.messagequeue.dto.VerwijderBetaalOpdrachtDto;
import nl.rivm.screenit.service.MessageService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.fasterxml.jackson.core.JsonProcessingException;

@Slf4j
@Service
public class CervixVerrichtingenQueueServiceImpl
{
	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private CervixHerindexeerVerrichtingenService herindexatieService;

	@Autowired
	private CervixVerwijderSepaDataService verwijderSepaDataService;

	@Autowired
	private MessageService messageService;

	private boolean bindSessionOnThread = false;

	private int heartbeatCounter = 0;

	@PostConstruct
	public void init()
	{

		LOG.info(
			"Queue gestart voor het verwerken van verrichtingen met de params: BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE={} en BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS= {}",
			Integer.getInteger("BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE", 500),
			Integer.getInteger("BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS", 200));

		ExecutorService executorService = Executors.newSingleThreadExecutor();
		executorService.submit(() ->
		{

			while (true)
			{
				herindexeer();

				verwijderBetaalOpdracht();
			}
		});

	}

	private void herindexeer()
	{
		bindSession();
		AtomicInteger totaalAantalVerrichtingen = new AtomicInteger(0);
		CervixHerindexatieDto herindexatieDto = null;
		try
		{
			Message message = messageService.getOldestMessage(MessageType.HERINDEXATIE);
			if (message != null)
			{
				herindexatieDto = messageService.getContent(message);
				totaalAantalVerrichtingen.set(0);
				verwerkHerindexeringMessage(herindexatieDto, totaalAantalVerrichtingen);
				messageService.dequeueMessage(message);
			}
		}
		catch (InterruptedException | JsonProcessingException e)
		{
			herindexatieService.logFout(herindexatieDto, totaalAantalVerrichtingen.get(), e);
			Thread.currentThread().interrupt();
		}
		finally
		{
			unbindSessionFactory();
		}
	}

	private void verwijderBetaalOpdracht()
	{
		bindSession();
		VerwijderBetaalOpdrachtDto verwijderBetaalOpdrachtDto = null;
		try
		{
			Message message = messageService.getOldestMessage(MessageType.VERWIJDER_BETAAL_OPDRACHT);
			if (message != null)
			{
				verwijderBetaalOpdrachtDto = messageService.getContent(message);
				verwerkVerwijderingMessage(verwijderBetaalOpdrachtDto);
				messageService.dequeueMessage(message);
			}

			unbindSessionFactory();
			wachtOpVolgendeOpdracht();
		}
		catch (Exception e)
		{
			LOG.error("Fout bij verwerking van verwijder betaal opdracht met id {}", (verwijderBetaalOpdrachtDto != null ? verwijderBetaalOpdrachtDto.getId() : "onbekend"),
				e);
		}
		finally
		{
			unbindSessionFactory();
		}
	}

	private void verwerkVerwijderingMessage(VerwijderBetaalOpdrachtDto opdrachtDto)
	{
		int batchSize = 1000;
		List<CervixBoekRegel> cervixBoekRegels = verwijderSepaDataService.haalBoekRegelsOp(opdrachtDto.getId(), batchSize);
		long teVerwerkenBoekregels = verwijderSepaDataService.aantalTeVerwerkenBoekregels(opdrachtDto.getId());
		while (!cervixBoekRegels.isEmpty())
		{
			LOG.info("{} van de {} regels opgehaald om te ontkoppelen", cervixBoekRegels.size(), teVerwerkenBoekregels);
			verwijderSepaDataService.ontkoppelBoekregelsVanSpecificatie(cervixBoekRegels);
			cervixBoekRegels = verwijderSepaDataService.haalBoekRegelsOp(opdrachtDto.getId(), batchSize);
		}

		CervixBetaalopdracht betaalopdracht = verwijderSepaDataService.haalBetaalOpdrachtOp(opdrachtDto.getId());
		verwijderSepaDataService.verwijderBetaalopdracht(betaalopdracht);
	}

	private void verwerkHerindexeringMessage(CervixHerindexatieDto herindexatieDto, AtomicInteger totaalAantalVerrichtingen) throws InterruptedException
	{
		herindexatieService.logStart(herindexatieDto);
		int aantalVerrichtingen;
		do
		{
			aantalVerrichtingen = herindexatieService.verrichtingenHerindexeren(herindexatieDto);
			totaalAantalVerrichtingen.addAndGet(aantalVerrichtingen);
			LOG.info("{} (verdere) verrichtingen zijn aangepast.", aantalVerrichtingen);
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
			LOG.warn("Interrupted!", e);
			Thread.currentThread().interrupt();
		}
		if (heartbeatCounter++ % 20 == 0)
		{
			LOG.info("Heartbeat verwerkVerrichtingVerzoeken");
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
			SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.unbindResource(sessionFactory);
			SessionFactoryUtils.closeSession(sessionHolder.getSession());
			bindSessionOnThread = false;
		}
	}
}
