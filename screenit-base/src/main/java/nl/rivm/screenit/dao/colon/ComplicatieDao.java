package nl.rivm.screenit.dao.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.SortState;

public interface ComplicatieDao
{

	long countComplicaties(Complicatie searchObject, List<Long> hierarchieCriteria);

	Iterator<Complicatie> getComplicaties(Complicatie searchObject, List<Long> hierarchieCriteria, int first, int count, SortState<String> sortState);

	List<Complicatie> geefAlleNietGekoppeldeComplicaties(Client client, Date date);
}
