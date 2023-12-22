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

import javax.annotation.Nullable;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.dto.ColonBlokkadeDto;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

public interface ColonBlokkadeService
{
	void createBlokkade(ColonBlokkadeDto blokkadeDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException;

	List<ColonBlokkade> splitBlokkade(ColonBlokkade unsavedObject, boolean alleKamers, ColoscopieCentrum intakelocatie);

	void logAction(ColonBlokkade unsavedObject, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis);

	String getPeriodeTekst(ColonBlokkade unsavedObject);

	void deleteBlokkade(Long blokkadeId, InstellingGebruiker instellingGebruiker) throws ValidatieException;

	Optional<ColonBlokkade> getBlokkade(Long blokkadeId);
}
