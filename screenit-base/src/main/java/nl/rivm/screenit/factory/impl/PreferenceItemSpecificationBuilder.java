package nl.rivm.screenit.factory.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.Getter;

import nl.rivm.screenit.model.PreferenceItem;
import nl.rivm.screenit.model.PreferenceItem_;
import nl.topicuszorg.preferencemodule.util.IPreferenceItemCriteriaBuilder;

import org.springframework.data.jpa.domain.Specification;

@Getter
public class PreferenceItemSpecificationBuilder implements IPreferenceItemCriteriaBuilder
{
	private Specification<PreferenceItem> specification;

	public PreferenceItemSpecificationBuilder(String key)
	{
		specification = (r, q, cb) -> cb.equal(r.get(PreferenceItem_.key), key);
	}

	@Override
	public PreferenceItemSpecificationBuilder withContextClass(String contextName)
	{
		specification = specification.and((r, q, cb) -> cb.equal(r.get(PreferenceItem_.contextClass), contextName));
		return this;
	}

	@Override
	public PreferenceItemSpecificationBuilder withoutContextClass()
	{
		specification = specification.and((r, q, cb) -> cb.isNull(r.get(PreferenceItem_.contextClass)));
		return this;
	}

	@Override
	public PreferenceItemSpecificationBuilder withContextId(String id)
	{
		specification = specification.and((r, q, cb) -> cb.equal(r.get(PreferenceItem_.contextId), id));
		return this;
	}

	@Override
	public PreferenceItemSpecificationBuilder withoutContextId()
	{
		specification = specification.and((r, q, cb) -> cb.isNull(r.get(PreferenceItem_.contextId)));
		return this;
	}
}
