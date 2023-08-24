package nl.rivm.screenit.dao.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;

import com.google.common.collect.Range;

public interface MammaBaseAfspraakDao
{
	List<MammaAfspraak> getNietGekoppeldeAfspraken(MammaCapaciteitBlok capaciteitsBlok);

	List<MammaAfspraak> getAfspraken(MammaScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	List<MammaAfspraak> getAfspraken(String seCode, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	long countAfspraken(long standplaatsPeriodeId, MammaAfspraakStatus... afspraakStatussen);

	Date[] getEersteEnLaatsteAfspraakMomenten(long standplaatsPeriodeiId, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	long countAfspraken(MammaStandplaats standplaats, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	long countAfspraken(MammaScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen);

	List<MammaAfspraak> getAfspraken(MammaStandplaats standplaats, Range<Date> periode, MammaAfspraakStatus... afspraakStatussen);

	Date readDatumVanOudsteNietAfgeslotenOnderzoek(LocalDate vandaag, String seCode);

	List<MammaAfspraak> readAfsprakenWaarvanOnderzoekNietIsDoorgevoerd(LocalDate vandaag, String seCode);
}
