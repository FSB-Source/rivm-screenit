package nl.rivm.screenit.dao.impl;

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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.ScreenitPreferenceItem;
import nl.topicuszorg.preferencemodule.context.GlobalContext;
import nl.topicuszorg.preferencemodule.dao.IContext;
import nl.topicuszorg.preferencemodule.model.PreferenceItem;

import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Primary 
@Repository
@Slf4j
@Transactional(propagation = Propagation.REQUIRED)
public class PreferenceDaoWithIdCacheImpl extends PreferenceDaoImpl
{
	private final Map<String, Long> keyIds = new ConcurrentHashMap<>();

	public PreferenceDaoWithIdCacheImpl()
	{
		LOG.info("Use PreferenceDaoWithIdCacheImpl instead of nl.topicuszorg.preferencemodule.PreferenceDaoImpl");
	}

	@Override
	public PreferenceItem getPreferenceItem(String key, IContext context)
	{
		if (!(context instanceof GlobalContext))
		{
			LOG.debug("no id-caching because non-global-context");
			return super.getPreferenceItem(key, context);
		}

		var keyId = keyIds.get(key);
		if (keyId == null)
		{
			LOG.debug("Id for key {} not in cache", key);
			return getItemAndCacheId(key, context);
		}

		return getItemViaCache(key, context, keyId);
	}

	private PreferenceItem getItemAndCacheId(String key, IContext context)
	{
		var preferenceItem = super.getPreferenceItem(key, context);
		if (preferenceItem != null && preferenceItem.getId() != null)
		{
			keyIds.put(key, (Long) preferenceItem.getId());
		}
		return preferenceItem;
	}

	private PreferenceItem getItemViaCache(String key, IContext context, Long keyId)
	{
		LOG.debug("Use id {} voor Key {} uit cache", keyId, key);
		var preferenceItem = getSession().get(ScreenitPreferenceItem.class, keyId); 
		if (preferenceItem == null)
		{
			LOG.debug("Cached Id {} for key {} not in database anymore", keyId, key);
			return getItemAndCacheId(key, context);
		}
		return preferenceItem;
	}
}
