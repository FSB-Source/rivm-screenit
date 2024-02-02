package nl.rivm.screenit.batch.jobs.brieven.genereren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.lang.reflect.ParameterizedType;

import nl.rivm.screenit.model.Brief;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractBrievenGenererenProcessor<B extends Brief> implements ItemProcessor<Long, B>
{
	@Autowired
	private HibernateService hibernateService;

	@Override
	public B process(Long item) throws Exception
	{
		return hibernateService.load(getBriefClass(), item);
	}

	@SuppressWarnings("unchecked")
	protected final Class<B> getBriefClass()
	{
		return (Class<B>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}
}
