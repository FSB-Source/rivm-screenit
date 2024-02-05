package nl.rivm.screenit.batch.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.model.colon.planning.VrijSlot;

public interface IntakeAfpraakService
{

	List<ClientAfspraak> getClientenVoorIntakeAfspraakMaken(StringBuilder foutmeldingTextUitJobContext);

	List<ClientAfspraak> getClientenVoorIntakeAfspraakMaken(Integer afstandFactor, Integer tijdFactor, StringBuilder foutTekst);

	List<VrijSlot> getAllVrijeSlotenIntakeafspraakperiode(int aantalGeselecteerdeClienten, LocalDate begintijd, LocalDate eindtijd, AtomicInteger aantalExtraDagen);

	double getWachttijdNormering(Integer intakeAfspraakPeriode, Integer tijdfactor);

	double getAfstandNormering(Integer maxAfstandClientColonscopiecentrum, Integer afstandfactor);

}
