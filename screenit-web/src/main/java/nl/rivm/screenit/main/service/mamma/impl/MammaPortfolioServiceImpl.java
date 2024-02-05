package nl.rivm.screenit.main.service.mamma.impl;

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

import nl.rivm.screenit.main.dao.mamma.MammaPortfolioDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaPortfolioZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaPortfolioService;
import nl.rivm.screenit.model.Client;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaPortfolioServiceImpl implements MammaPortfolioService
{
	@Autowired
	private MammaPortfolioDao portfolioDao;

	@Override
	public List<Client> zoekPortfolioClienten(MammaPortfolioZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return portfolioDao.zoekPortfolioClienten(zoekObject, first, count, sortProperty, ascending);
	}

	@Override
	public List<Long> zoekPortfolioClientenIds(MammaPortfolioZoekObject zoekObject, String sortProperty, boolean ascending)
	{
		return portfolioDao.zoekPortfolioClientenIds(zoekObject, sortProperty, ascending);
	}

	@Override
	public long countPortfolioClienten(MammaPortfolioZoekObject zoekObject)
	{
		return portfolioDao.countPortfolioClienten(zoekObject);
	}
}
