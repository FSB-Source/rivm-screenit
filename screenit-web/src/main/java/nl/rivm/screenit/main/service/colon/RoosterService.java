package nl.rivm.screenit.main.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakslotStatus;
import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;

import com.google.common.collect.Range;

public interface RoosterService
{
	List<ColonAfspraakslotListViewWrapper> getAlleAfspraakslotsInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie);

	List<ColonTijdslotDto> searchTijdslots(RoosterListViewFilter filter, long intakelocatieId, ColonTijdslotType typeTijdslot);

	List<ColonAfspraakslotListViewWrapper> getAfspraakslots(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie);

	long getAfspraakslotsCount(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie);

	ColonAfspraakslotStatus getAfspraakslotStatus(ColonAfspraakslot afspraakslot);

	Optional<ColonAfspraakslot> getAfspraakslot(Long id);

	Integer getCurrentAantalAfspraakslots(ColonIntakelocatie intakeLocatie, Range<LocalDateTime> periode);

	List<ColonBlokkade> getBlokkades(Range<LocalDateTime> range, List<ColonIntakekamer> kamers);

	List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColonIntakelocatie intakelocatie);

	long getBlokkadesCount(RoosterListViewFilter filter, ColonIntakelocatie intakelocatie);

	void valideerTijdslot(ColonTijdslot tijdslot) throws ValidatieException;

	ColonIntakelocatie getIntakelocatieVanInstellingGebruiker(InstellingGebruiker instellingGebruiker);

	List<ColonAfspraakslot> getAfspraakslotsInRangeEnKamer(Range<LocalDateTime> range, ColonAfspraakslot afspraakslot);

	List<ColonAfspraakslot> getAfspraakslotsInRange(Range<LocalDate> range);

	<S extends ColonTijdslot> List<S> maakHerhalingTijdslotsAan(S tijdslot, ColonHerhalingDto herhalingDto);

	Range<LocalDateTime> getCurrentViewRange(ColonTijdslotDto tijdslot);

	Range<LocalDateTime> getCurrentViewRange(ColonTijdslot tijdslot);
}
