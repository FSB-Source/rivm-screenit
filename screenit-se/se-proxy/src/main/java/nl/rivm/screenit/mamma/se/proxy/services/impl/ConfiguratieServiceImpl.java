package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.dao.ConfiguratieDao;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ConfiguratieServiceImpl implements ConfiguratieService
{

	@Autowired
	private ConfiguratieDao configuratieDao;

	@Override
	public String getConfiguratieValue(SeConfiguratieKey key)
	{
		return configuratieDao.getConfiguratieValue(key);
	}

	@Override
	public Integer getConfiguratieIntegerValue(SeConfiguratieKey key)
	{
		return configuratieDao.getConfiguratieIntegerValue(key);
	}

	@Override
	public void insertOrUpdateConfiguratieValue(SeConfiguratieKey key, String value)
	{
		configuratieDao.insertOrUpdateConfiguratieValue(key, value);
	}

	@Override
	public void insertOrUpdateConfiguratieValues(Map<SeConfiguratieKey, String> parameters)
	{
		for (Map.Entry<SeConfiguratieKey, String> parameter : parameters.entrySet())
		{
			insertOrUpdateConfiguratieValue(parameter.getKey(), parameter.getValue());
		}
	}
}
