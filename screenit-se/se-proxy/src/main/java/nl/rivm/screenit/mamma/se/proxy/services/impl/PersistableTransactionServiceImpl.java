package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.dao.PersistableTransactionDao;
import nl.rivm.screenit.mamma.se.proxy.model.PersistableTransaction;
import nl.rivm.screenit.mamma.se.proxy.services.PersistableTransactionService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PersistableTransactionServiceImpl implements PersistableTransactionService
{
    private static final Logger LOG = LoggerFactory.getLogger(PersistableTransactionServiceImpl.class);

    @Autowired
    private PersistableTransactionDao persistableTransactionDao;

	public void putLast(PersistableTransaction transaction)
	{
        persistableTransactionDao.putLast(transaction);
    }

	public PersistableTransaction takeFirst()
	{
        return persistableTransactionDao.takeFirst();
    }

	public List<PersistableTransaction> getAll()
	{
        return persistableTransactionDao.getAll();
    }

	public void remove(Long id)
	{
        persistableTransactionDao.remove(id);
    }

	public void addToVerstuurdeTransacties(PersistableTransaction transaction)
	{
        persistableTransactionDao.addToVerstuurdeTransacties(transaction);
    }

    public void clearOldEntries()
    {
        persistableTransactionDao.startOfDayCleanUp();
    }

    public void addToFouteTransactie(PersistableTransaction transaction)
    {
        persistableTransactionDao.addToFouteTransactie(transaction);
    }
}
