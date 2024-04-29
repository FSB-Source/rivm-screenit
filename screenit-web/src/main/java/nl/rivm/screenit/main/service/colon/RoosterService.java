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

import java.util.Date;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.dao.colon.IntakelocatieVanTotEnMetFilter;
import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.util.Periode;

import com.google.common.collect.Range;

public interface RoosterService
{

	List<RoosterItem> getRooster(Periode periode, List<Kamer> kamers);

	void magRoosterItemOpslaanVerwijderen(RoosterItem roosteritem, Range<Date> currentViewRange, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException;

	void magBlokkadeOpslaanVerwijderen(ColonBlokkade blokkade, RecurrenceOption recurrenceOption, Date recurrenceEditEnd, Date origRecEndDateTime, boolean wijzigen,
		List<Kamer> kamers) throws HeeftAfsprakenException;

	void toevoegenHerhaling(AbstractAppointment appointment);

	List<RoosterItemListViewWrapper> getAlleRoosterBlokkenInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	List<ColonTijdslotDto> searchTijdslots(RoosterListViewFilter filter, long intakelocatieId, ColonTijdSlotType typeTijdslot);

	List<RoosterItemListViewWrapper> getRoosterBlokken(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	long getRoosterBlokkenCount(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	RoosterItemStatus getRoosterItemStatus(RoosterItem roosterItem);

	Optional<RoosterItem> getRoosterItem(Long id);

	Integer getCurrentAantalRoosterBlokken(ColoscopieCentrum intakeLocatie, Range<Date> periode);

	List<Date> getMdlDatums(Client client, IntakelocatieVanTotEnMetFilter intakeVanTotEnMetFilter);

	List<ColonBlokkade> getBlokkades(Periode periode, List<Kamer> kamers);

	List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakelocatie);

	long getBlokkadesCount(RoosterListViewFilter filter, ColoscopieCentrum intakelocatie);

	void valideerTijdslot(AbstractAppointment tijdslot) throws ValidatieException;

	ColoscopieCentrum getIntakelocatieVanInstellingGebruiker(InstellingGebruiker instellingGebruiker);

	List<Range<Date>> getNieuweTijdSloten(AbstractAppointment tijdslot, Range<Date> currentViewRange);

	Range<Date> getCurrentViewRange(AbstractAppointment tijdslot, RecurrenceOption recurrenceOption, Date recurrenceEditEnd, Date origRecEndDateTime);

	List<RoosterItem> splitAfspraakslot(RoosterItem unsavedObject, Integer aantalBlokken, ColoscopieCentrum intakelocatie);

	List<RoosterItem> getAfspraakslotsInRangesEnKamer(List<Range<Date>> ranges, RoosterItem afspraakslot);

	List<RoosterItem> getAfspraakslotsInRange(Range<Date> range);

	<S extends AbstractAppointment> List<S> maakHerhalingTijdslotsAan(S tijdslot, ColonHerhalingDto herhalingDto);
}
