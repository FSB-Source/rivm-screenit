package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.factory.impl.PreferenceItemSpecificationBuilder;
import nl.rivm.screenit.model.PreferenceItem;
import nl.rivm.screenit.repository.algemeen.PreferenceItemRepository;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.context.GlobalContext;
import nl.topicuszorg.preferencemodule.dao.IContext;
import nl.topicuszorg.preferencemodule.dao.PreferenceItemDao;
import nl.topicuszorg.preferencemodule.model.IPreferenceItem;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Repository
public class PreferenceDaoImpl extends AbstractAutowiredDao implements PreferenceItemDao
{
	private final Map<String, Long> keyIds = new ConcurrentHashMap<>();

	@Autowired
	private PreferenceItemRepository preferenceItemRepository;

	@Override
	public IPreferenceItem getPreferenceItem(String key, IContext context)
	{
		if (!(context instanceof GlobalContext))
		{
			LOG.debug("no id-caching because non-global-context");
			return getPreferenceItemLocal(key, context);
		}

		var keyId = keyIds.get(key);
		if (keyId == null)
		{
			LOG.debug("Id for key {} not in cache", key);
			return getItemAndCacheId(key, context);
		}

		return getItemViaCache(key, context, keyId);
	}

	private IPreferenceItem getItemAndCacheId(String key, IContext context)
	{
		var preferenceItem = getPreferenceItemLocal(key, context);
		if (preferenceItem != null && preferenceItem.getId() != null)
		{
			keyIds.put(key, (Long) preferenceItem.getId());
		}
		return preferenceItem;
	}

	private IPreferenceItem getItemViaCache(String key, IContext context, Long keyId)
	{
		LOG.debug("Use id {} voor Key {} uit cache", keyId, key);
		var preferenceItem = getSession().get(PreferenceItem.class, keyId); 
		if (preferenceItem == null)
		{
			LOG.debug("Cached Id {} for key {} not in database anymore", keyId, key);
			return getItemAndCacheId(key, context);
		}
		return preferenceItem;
	}

	@Override
	public Optional<String> getPreference(String key, IContext context)
	{
		return getPreferenceOptional(getPreferenceItem(key, context));
	}

	@Override
	@Transactional
	public void saveOrUpdate(IPreferenceItem preferenceItem)
	{
		preferenceItemRepository.save((PreferenceItem) preferenceItem);
	}

	private Optional<String> getPreferenceOptional(IPreferenceItem preferenceItem)
	{
		return preferenceItem == null ? Optional.empty() : Optional.ofNullable(preferenceItem.getValue());
	}

	private IPreferenceItem getPreferenceItemLocal(String key, IContext context)
	{
		var criteria = new PreferenceItemSpecificationBuilder(key);
		context.addContextToPreferenceItemCriteriaBuilder(criteria);
		return preferenceItemRepository.findOne(criteria.getSpecification()).orElse(null);
	}
}
