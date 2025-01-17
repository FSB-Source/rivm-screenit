package nl.rivm.screenit.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.InputStream;

import javax.mail.internet.MimeMessage;

import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessagePreparator;

public class NoopMailSender implements JavaMailSender
{

	@Override
	public void send(SimpleMailMessage simpleMessage)
	{
	}

	@Override
	public void send(SimpleMailMessage[] simpleMessages)
	{
	}

	@Override
	public MimeMessage createMimeMessage()
	{
		return null;
	}

	@Override
	public MimeMessage createMimeMessage(InputStream contentStream)
	{
		return null;
	}

	@Override
	public void send(MimeMessage mimeMessage)
	{

	}

	@Override
	public void send(MimeMessage[] mimeMessages)
	{

	}

	@Override
	public void send(MimeMessagePreparator mimeMessagePreparator)
	{

	}

	@Override
	public void send(MimeMessagePreparator[] mimeMessagePreparators)
	{

	}

}
