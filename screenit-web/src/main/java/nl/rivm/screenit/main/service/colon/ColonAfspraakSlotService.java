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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakSlotDto;
import nl.rivm.screenit.model.colon.planning.RoosterItem;

public interface ColonAfspraakSlotService
{
	List<RoosterItem> splitAfspraakSlot(RoosterItem unsavedObject, Integer aantalBlokken, ColoscopieCentrum intakelocatie);

	void createAfspraakSlot(ColonAfspraakSlotDto afspraakSlotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException;

	void checkEindTijdOpZelfdeDag(LocalDateTime startDateTime, LocalDateTime endDateTime, ColoscopieCentrum intakelocatie) throws ValidatieException;

	void checkCapaciteitBerekening(RoosterItem roosterItem, ColoscopieCentrum intakelocatie) throws ValidatieException;

	void updateAfspraakSlot(Long id, ColonAfspraakSlotDto afspraakSlotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException;

	void deleteAfspraakSlot(Long id, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException;

}
