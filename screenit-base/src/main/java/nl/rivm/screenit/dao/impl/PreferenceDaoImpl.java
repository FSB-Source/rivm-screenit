package nl.rivm.screenit.dao.impl;

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

import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.dao.IContext;
import nl.topicuszorg.preferencemodule.dao.PreferenceItemDao;
import nl.topicuszorg.preferencemodule.model.PreferenceItem;
import nl.topicuszorg.preferencemodule.until.PreferenceItemCriteria;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Optional;

@Repository
@Transactional(propagation = Propagation.REQUIRED)
public class PreferenceDaoImpl extends AbstractAutowiredDao implements PreferenceItemDao
{

	@Override
	public Optional<String> getPreference(String key, IContext context)
	{
		return getPreferenceOptional(getPreferenceItem(key, context));
	}

	private Optional<String> getPreferenceOptional(PreferenceItem preferenceItem)
	{
		return preferenceItem == null ? Optional.<String> absent() : Optional.fromNullable(preferenceItem.getValue());
	}

	@Override
	public void saveOrUpdate(PreferenceItem preferenceItem)
	{
		getSession().saveOrUpdate(preferenceItem);
	}

	@Override
	public PreferenceItem getPreferenceItem(String key, IContext context)
	{
		PreferenceItemCriteria criteria = new PreferenceItemCriteria(key);
		context.addContextToPreferenceItemCriteria(criteria);
		return criteria.uniqueResult(getSession());
	}
}
