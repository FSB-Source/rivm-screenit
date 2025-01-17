package nl.rivm.screenit.main.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.BeperkingException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.BulkVerwijderenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakslotStatus;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;

import com.google.common.collect.Range;

public interface ColonAfspraakslotService
{
	void createAfspraakslot(ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException, BulkAanmakenException;

	void checkEindTijdOpZelfdeDag(LocalDateTime startDateTime, LocalDateTime endDateTime, ColonIntakelocatie intakelocatie) throws ValidatieException;

	void checkCapaciteitBerekening(ColonAfspraakslot afspraakslot, ColonIntakelocatie intakelocatie) throws ValidatieException;

	void updateAfspraakslot(Long id, ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException;

	void deleteAfspraakslot(Long id, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException;

	void bulkDeleteAfspraakslots(List<Long> id, InstellingGebruiker instellingGebruiker, boolean alleenValidatie)
		throws BulkVerwijderenException;

	List<ColonAfspraakslotDto> getAfspraakslots(LocalDate startDate, LocalDate endDate, ColonIntakelocatie intakeLocatie);

	List<ColonTijdslotDto> searchAfspraakslots(RoosterListViewFilter filter, long intakelocatieId);

	ColonAfspraakslotStatus getAfspraakslotStatus(ColonAfspraakslot afspraakslot);

	Integer getCurrentAantalAfspraakslots(ColonIntakelocatie intakeLocatie, Range<LocalDateTime> periode);

	void valideerBeperkingen(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType) throws BeperkingException;
}
