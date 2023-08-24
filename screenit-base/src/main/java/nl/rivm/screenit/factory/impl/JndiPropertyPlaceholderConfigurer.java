package nl.rivm.screenit.factory.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Properties;

import javax.naming.NamingException;

import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;
import org.springframework.jndi.JndiTemplate;

public class JndiPropertyPlaceholderConfigurer extends PropertyPlaceholderConfigurer
{

	private String jndiPrefix = "";

	private JndiTemplate jndiTemplate = new JndiTemplate();

	@Override
	protected String resolvePlaceholder(String placeholder, Properties props)
	{
		String value = null;
		value = super.resolvePlaceholder(placeholder, props);
		if (value == null)
		{
			value = resolveJndiPlaceholder(placeholder);
		}
		return value;
	}

	private String resolveJndiPlaceholder(String placeholder)
	{
		try
		{
			return jndiTemplate.lookup(jndiPrefix + placeholder, String.class);
		}
		catch (NamingException e)
		{

		}
		return null;
	}

	public void setJndiPrefix(String jndiPrefix)
	{
		this.jndiPrefix = jndiPrefix;
	}

	public void setJndiTemplate(JndiTemplate jndiTemplate)
	{
		this.jndiTemplate = jndiTemplate;
	}
}
