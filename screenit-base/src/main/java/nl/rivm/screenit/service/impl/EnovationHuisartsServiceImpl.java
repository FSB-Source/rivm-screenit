package nl.rivm.screenit.service.impl;

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

import java.util.List;

import nl.rivm.screenit.dao.EnovationHuisartsDao;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.service.EnovationHuisartsService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class EnovationHuisartsServiceImpl implements EnovationHuisartsService
{
	private static final Logger LOG = LoggerFactory.getLogger(EnovationHuisartsServiceImpl.class);

	@Autowired
	private EnovationHuisartsDao huisartsDao;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdate(EnovationHuisarts huisarts)
	{
		huisartsDao.saveOrUpdate(huisarts);
	}

	@Override
	public EnovationHuisarts getHuisartsByKlantnummer(String klantnummer)
	{
		return huisartsDao.getHuisartsByKlantnummer(klantnummer);
	}

	@Override
	public List<EnovationHuisarts> zoekHuisartsen(EnovationHuisarts zoekObject, String sortProperty, boolean ascending, int first, int count)
	{
		return huisartsDao.zoekHuisartsen(zoekObject, sortProperty, ascending, first, count);
	}

	@Override
	public long telHuisartsen(EnovationHuisarts zoekObject)
	{
		return huisartsDao.telHuisartsen(zoekObject);
	}

	@Override
	public int valideerKlantnummers(List<String> klantnummers)
	{
		List<String> activeKlantnummers = huisartsDao.getKlantnummersVanAlleActiveHuisartens();
		activeKlantnummers.removeAll(klantnummers);
		if (!activeKlantnummers.isEmpty())
		{
			LOG.info("Huisarten verwijderd met volgende klantnummer: " + activeKlantnummers);
			huisartsDao.verwijderHuisartsen(activeKlantnummers);
		}
		return activeKlantnummers.size();
	}

}
