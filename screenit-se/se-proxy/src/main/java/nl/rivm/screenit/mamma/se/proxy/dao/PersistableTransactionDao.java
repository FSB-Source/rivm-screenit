package nl.rivm.screenit.mamma.se.proxy.dao;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.model.PersistableTransaction;

import java.util.List;

public interface PersistableTransactionDao {
    void putLast(PersistableTransaction transaction);

    PersistableTransaction takeFirst();

    List<PersistableTransaction> getAll();

    void remove(Long id);

    void addToVerstuurdeTransacties(PersistableTransaction transaction);

    void startOfDayCleanUp();

    void addToFouteTransactie(PersistableTransaction transaction);

    void clearDb();
}
