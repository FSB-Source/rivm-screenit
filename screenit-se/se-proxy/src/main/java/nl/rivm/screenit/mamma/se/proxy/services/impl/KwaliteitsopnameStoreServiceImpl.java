package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.mamma.se.proxy.services.KwaliteitsopnameStoreService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class KwaliteitsopnameStoreServiceImpl implements KwaliteitsopnameStoreService
{
	private static final Logger LOG = LoggerFactory.getLogger(KwaliteitsopnameStoreServiceImpl.class);

	private Map<Integer, Integer> laatstUitgegevenVolgnrByMammograafnr = new HashMap<>();

	synchronized public int geefVolgnrUit(int mammograafnummer)
	{
		Integer laatstUitgegevenVolgnr = laatstUitgegevenVolgnrByMammograafnr.get(mammograafnummer);
		Integer result = laatstUitgegevenVolgnr != null ? laatstUitgegevenVolgnr + 1 : 1;
		if (result > 99)
		{
			LOG.error("Meer dan 99 (namelijk " + result + ") kwaliteitsopnamen gestart voor mammograaf nummer " + mammograafnummer);
		}
		laatstUitgegevenVolgnrByMammograafnr.put(mammograafnummer, result);
		return result;
	}

	public void resetVolgnrs()
	{
		laatstUitgegevenVolgnrByMammograafnr = new HashMap<>();
	}
}
