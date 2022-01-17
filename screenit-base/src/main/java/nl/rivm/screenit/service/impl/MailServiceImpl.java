package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.annotation.Nonnull;

import nl.rivm.screenit.model.Mail;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.repository.MailRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MailServiceImpl implements MailService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MailRepository mailRepository;

	@Override
	public void queueMail(String to, String subject, String content)
	{
		queueMail(to, subject, content, MailPriority.NORMAL);
	}

	@Override
	public void queueMail(String to, String subject, String content, @Nonnull MailPriority priority)
	{
		mailRepository.save(
			new Mail(currentDateSupplier.getLocalDateTime(),
				to,
				subject,
				content,
				priority));
	}
}
