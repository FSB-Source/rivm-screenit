
package nl.rivm.screenit.main.service.impl;

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

import java.util.List;

import nl.rivm.screenit.main.dao.HoudbaarheidDao;
import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.model.AbstractHoudbaarheid;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class HoudbaarheidServiceImpl implements HoudbaarheidService
{

	@Autowired
	private HoudbaarheidDao houdbaarheidDao;

	@Override
	public <H extends AbstractHoudbaarheid> List<H> getHoudbaarheidItems(Class<H> clazz, long first, long count, String sortProperty, boolean asc)
	{
		return houdbaarheidDao.getHoudbaarheidItems(clazz, first, count, sortProperty, asc);
	}

	@Override
	public <H extends AbstractHoudbaarheid> Long countHoudbaarheidItems(Class<H> clazz)
	{
		return houdbaarheidDao.countHoudbaarheidItems(clazz);
	}

	@Override
	public <H extends AbstractHoudbaarheid> boolean overlaptBestaandeReeks(H vervalDatum)
	{
		return CollectionUtils.isNotEmpty(houdbaarheidDao.overlaptBestaandeReeks(vervalDatum));
	}

}
