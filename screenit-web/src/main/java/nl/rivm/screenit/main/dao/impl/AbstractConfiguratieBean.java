package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import javax.inject.Inject;

import nl.rivm.screenit.PreferenceKey;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Qualifier;

public abstract class AbstractConfiguratieBean
{
	@Inject
	private SimplePreferenceService preferenceService;

	@Inject
	@Qualifier("applicationEnvironment")
	private String applicationEnvironment;

	protected String getStringValue(PreferenceKey prefKey, String defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getString(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	protected boolean getBooleanValue(PreferenceKey prefKey, Boolean defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getBoolean(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	protected int getIntegerValue(PreferenceKey prefKey, Integer defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getInteger(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	private boolean useDbPreferences()
	{
		return !"Filler".equals(applicationEnvironment) && !"Unittest".equals(applicationEnvironment);
	}
}
