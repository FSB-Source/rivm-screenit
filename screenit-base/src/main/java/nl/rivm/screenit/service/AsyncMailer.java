package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.Client;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class AsyncMailer extends OpenHibernate5SessionInThread
{
	@Autowired
	protected HibernateService hibernateService;

	private static final Logger LOG = LoggerFactory.getLogger(AsyncMailer.class);

	private final String mailContent;

	private final String toMail;

	private final String subject;

	@Autowired
	private MailService mailService;

	public AsyncMailer(String toMail, String subject, String mailContent)
	{
		this.toMail = toMail;
		this.subject = subject;
		this.mailContent = mailContent;
	}

	@Override
	protected void runInternal()
	{
		LOG.info("Verstuur async mail naar {}, inhoud: {}", toMail, mailContent);

		if (mailService.sendEmail(toMail, subject, mailContent))
		{
			onSuccess();
		}
		else
		{
			onError();
		}
	}

	public void onError()
	{
	}

	public void onSuccess()
	{
	}

	protected Client getClient(long clientid)
	{
		return hibernateService.getHibernateSession().get(Client.class, clientid);
	}
}
