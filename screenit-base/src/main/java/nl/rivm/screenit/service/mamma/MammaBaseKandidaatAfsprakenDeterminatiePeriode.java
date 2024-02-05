package nl.rivm.screenit.service.mamma;

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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaKandidaatAfspraak;
import nl.rivm.screenit.service.mamma.impl.MammaOnvoldoendeVrijeCapaciteitException;

public interface MammaBaseKandidaatAfsprakenDeterminatiePeriode
{

	LocalDate getHuidigeDagVoorPlannenAfspraken();

	List<MammaKandidaatAfspraak> getKandidaatAfspraken(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet,
		boolean extraOpties, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, boolean corrigeerNegatieveVrijeCapaciteit);

	MammaKandidaatAfspraak getKandidaatAfspraakBulkVerzetten(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet,
		BigDecimal opkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen);

	MammaKandidaatAfspraak getKandidaatAfspraakUitnodiging(MammaDossier dossier, MammaStandplaatsRonde standplaatsRonde, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, Integer afspraakBijUitnodigenVanafAantalWerkdagen)
		throws MammaOnvoldoendeVrijeCapaciteitException;
}
