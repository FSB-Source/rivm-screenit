package nl.rivm.screenit.dao.mamma;

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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.topicuszorg.organisatie.model.Adres;

public interface MammaBaseTehuisClientenDao
{
	long countClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres);

	List<Client> getClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres);

	List<Client> getClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres, int first, int count, String sortProperty, boolean isAscending);
}
