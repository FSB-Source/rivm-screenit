package nl.rivm.screenit.main.dao.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;

public interface MammaUitwisselportaalDao
{

	List<MammaDownloadOnderzoekenVerzoek> searchVerzoeken(MammaDownloadOnderzoekenVerzoek searchObject, long first, long count, String sortProperty, boolean asc);

	long countVerzoeken(MammaDownloadOnderzoekenVerzoek searchObject);

	List<MammaDownloadOnderzoekenVerzoek> getDownloadVerzoekenGedownload(MammaOnderzoek onderzoek);

	Instelling getLaatstGedownloadDoorInstelling(MammaDossier dossier);
}
