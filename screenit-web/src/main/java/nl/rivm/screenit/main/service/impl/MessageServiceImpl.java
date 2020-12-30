package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Locale;

import nl.rivm.screenit.main.service.LocaleResolver;
import nl.rivm.screenit.main.service.LocaleResolverService;
import nl.rivm.screenit.main.service.MessageService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;
import org.springframework.stereotype.Service;

@Service
public class MessageServiceImpl implements MessageService
{

	@Autowired
	private LocaleResolverService localeResolverService;

	@Autowired
	private MessageSource messageSource;

	@Override
	public String getMessage(String code, Object... args)
	{
		LocaleResolver resolver = localeResolverService.getLocaleResolver();
		Locale locale = null;

		if (resolver != null)
		{
			locale = resolver.getLocale();
		}

		String result;

		try
		{
			result = messageSource.getMessage(code, args, locale);
		}
		catch (NoSuchMessageException nsme)
		{
			result = "[unknown key " + code + "]";
		}

		return result;

	}

}
