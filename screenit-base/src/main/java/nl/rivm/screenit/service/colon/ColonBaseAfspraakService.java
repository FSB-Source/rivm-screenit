package nl.rivm.screenit.service.colon;

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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.enums.BriefType;

import com.google.common.collect.Range;

public interface ColonBaseAfspraakService
{
	void verplaatsAfspraak(ColonIntakeAfspraak nieuweAfspraak, Account account, BriefType briefType, boolean briefTegenhouden, boolean uitRooster,
		boolean verwezenMedischeRedenenDoorInfolijn);

	void annuleerAfspraak(ColonIntakeAfspraak afspraak, Account account, ColonAfspraakStatus status, boolean communicatieTegenhouden);

	void maakNieuweAfspraak(Client client, ColonIntakeAfspraak nieuweAfspraak, boolean briefTegenhouden, boolean uitRooster,
		BriefType briefType, Account account);

	List<ColonIntakeAfspraak> getAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie, long first, long count,
		String property, boolean ascending);

	long countAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie);

	void verzendHuisartsBerichtOpnieuw(Client client, Account account);

	boolean magWijzigenAfzeggen(ColonIntakeAfspraak afspraak);

	boolean magNieuweAfspraakMaken(Client client);

	boolean heeftOnafgerondeVerwijzingOmMedischeRedenen(ColonIntakeAfspraak afspraak);

	List<ColonIntakeAfspraak> getAfsprakenKamersInRange(ColonIntakekamer kamer, Range<LocalDateTime> range);

	ColonAfspraakslot getAfspraakslotVoorAfspraak(ColonIntakeAfspraak newAfspraak);

	ColonAfspraakslot getVrijAfspraakslotVoorAfspraak(ColonIntakeAfspraak newAfspraak);

	void setAfspraakStatus(ColonIntakeAfspraak afspraak, ColonAfspraakStatus status);

	void afspraakAfzeggen(ColonIntakeAfspraak afspraak, ColonAfspraakStatus status, LocalDateTime nu, boolean communicatieTegenhouden);

	boolean isDoorverwezenOmMedischeRedenenZonderNieuweAfspraak(Client client);

	boolean isAfspraakVerwezenOmMedischeRedenen(ColonIntakeAfspraak afspraak);

	ColonIntakeAfspraak zoekBevestigdeDoorverwijzendeAfspraak(ColonIntakeAfspraak afspraak);

	boolean heeftClientIntakeAfspraakMetConclusieBezwaar(String bsn);
}
