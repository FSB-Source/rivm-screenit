package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.transform.ResultTransformer;

public class MammaBeoordelingResultTransformer implements ResultTransformer
{

	@Override
	public AbstractHibernateObject transformTuple(Object[] tuple, String[] aliases)
	{
		return (AbstractHibernateObject) tuple[0];
	}

	@Override
	public List transformList(List list)
	{
		return list;
	}
}
