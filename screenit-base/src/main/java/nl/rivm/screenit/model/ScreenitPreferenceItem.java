package nl.rivm.screenit.model;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.preferencemodule.model.PreferenceItem;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Audited
@Table(schema = "algemeen")
@Entity(name = "pref_prefitem")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ScreenitPreferenceItem extends AbstractHibernateObject implements PreferenceItem
{

	private String contextClass;

	private String contextId;

	private String key;

	@Column(length = 4000)
	private String value;

	public String getContextClass()
	{
		return this.contextClass;
	}

	@Override
	public void setContextClass(String contextClass)
	{
		this.contextClass = contextClass;
	}

	@Override
	public String getContextId()
	{
		return this.contextId;
	}

	@Override
	public void setContextId(String contextId)
	{
		this.contextId = contextId;
	}

	@Override
	public String getKey()
	{
		return this.key;
	}

	@Override
	public void setKey(String key)
	{
		this.key = key;
	}

	@Override
	public String getValue()
	{
		return value;
	}

	@Override
	public void setValue(String value)
	{
		this.value = value;
	}
}
