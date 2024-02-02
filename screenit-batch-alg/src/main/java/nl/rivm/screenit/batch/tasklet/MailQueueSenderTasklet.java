package nl.rivm.screenit.batch.tasklet;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import javax.mail.MessagingException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.MailSenderService;
import nl.rivm.screenit.model.Mail;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.MailRepository;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.mail.MailException;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Slf4j
@Configuration
@EnableScheduling
public class MailQueueSenderTasklet
{
	@Autowired
	private MailRepository mailRepository;

	@Autowired
	private MailSenderService mailSenderService;

	@Autowired
	private LogService logService;

	private static final Pageable BATCH_SIZE = PageRequest.of(0, 500);

	private static final int QUEUE_WARNING_THRESHOLD = 5000;

	private static final int VERSTUURPROBLEMEN_MAX_COUNT = 5;

	private static final int VERSTUURPROBLEMEN_DELAY = 10000;

	private static int verstuurproblemenCount = 0;

	private static boolean verstuurproblemen = false;

	private boolean queueSizeWarning = false;

	@Scheduled(fixedDelay = 10000)
	public void handleMails()
	{
		LOG.info("Heartbeat mailQueueSenderTasklet");
		try
		{
			wachtBijProblemen();
			OpenHibernate5Session.withoutTransaction().run(() -> verwerkMailQueue());
		}
		catch (Exception e)
		{
			logVerstuurProblemen(e);
			return;
		}
		logVerstuurProblemenOpgelost();
	}

	private void wachtBijProblemen() throws InterruptedException
	{
		if (verstuurproblemen)
		{
			var wachttijd = verstuurproblemenCount * VERSTUURPROBLEMEN_DELAY;
			LOG.info("Er zijn problemen met het versturen van mail, wacht {} seconden extra.", wachttijd / 1000);
			Thread.sleep(wachttijd);
		}
	}

	private void verwerkMailQueue() throws MessagingException, MailException
	{
		var queueSize = mailRepository.count();
		for (Mail mail : mailRepository.findAllByOrderByPriorityAscIdAsc(BATCH_SIZE))
		{
			mailSenderService.verzendEnVerwijderQueuedMail(mail);
		}
		logQueueSizeProblemen(queueSize);
	}

	private void logQueueSizeProblemen(Long queueSize)
	{
		var queueSizeWarningActive = queueSizeWarning;
		queueSizeWarning = queueSize > QUEUE_WARNING_THRESHOLD;
		if (queueSizeWarningActive != queueSizeWarning)
		{
			if (queueSizeWarning)
			{
				LOG.warn("Queue size wordt te groot!");
				logService.logGebeurtenis(LogGebeurtenis.MAIL_QUEUE_ERG_GROOT, null, String.format("Er staan meer dan %d berichten in de queue", QUEUE_WARNING_THRESHOLD));
			}
			else
			{
				LOG.info("Queue size wordt weer klein genoeg");
				logService.logGebeurtenis(LogGebeurtenis.MAIL_QUEUE_NORMAAL, null, "Het aantal berichten in de queue is weer normaal.");
			}
		}
	}

	private void logVerstuurProblemenOpgelost()
	{
		if (verstuurproblemenCount > 0)
		{
			logService.logGebeurtenis(LogGebeurtenis.MAIL_QUEUE_VERBINDING_HERSTELD, null, "Mailberichten worden weer succesvol verstuurd.");
		}
		verstuurproblemen = false;
		verstuurproblemenCount = 0;
	}

	private void logVerstuurProblemen(Exception exception)
	{
		LOG.error("Runtime exceptie tijdens het versturen van mailberichten.", exception);
		if (!verstuurproblemen)
		{
			logService.logGebeurtenis(LogGebeurtenis.MAIL_QUEUE_VERSTUREN_MISLUKT, null,
				"Er is een onbekende fout opgetreden tijdens het versturen van mailberichten, neem contact op met Topicus.");
		}
		verstuurproblemen = true;
		if (verstuurproblemenCount < VERSTUURPROBLEMEN_MAX_COUNT)
		{
			verstuurproblemenCount++;
		}
	}
}
