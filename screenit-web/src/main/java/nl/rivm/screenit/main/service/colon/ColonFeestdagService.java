package nl.rivm.screenit.main.service.colon;

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

import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.colon.ColonFeestdag;
import nl.rivm.screenit.model.colon.dto.ColonFeestdagDto;

public interface ColonFeestdagService
{
	List<ColonFeestdag> getFeestdagen();

	Optional<ColonFeestdag> getFeestdagById(Long id);

	ColonFeestdag updateFeestdag(Long id, ColonFeestdagDto feestdag) throws ValidatieException;

	ColonFeestdag createFeestdag(ColonFeestdagDto feestdag) throws ValidatieException;

	void deleteFeestdag(Long id) throws ValidatieException;
}
