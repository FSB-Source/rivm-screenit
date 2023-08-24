
package nl.rivm.screenit.main.dao;

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

import java.util.Iterator;

import nl.rivm.screenit.main.model.colon.IFobtBatchFilter;
import nl.rivm.screenit.model.colon.IFOBTBestand;

public interface IfobtBestandDao
{

	Iterator<? extends IFOBTBestand> getBestanden(IFobtBatchFilter filter, long first, long count, String sortProperty, boolean ascending);

	long countBestanden(IFobtBatchFilter filter);

}
