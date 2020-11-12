package nl.rivm.screenit.dao.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Collection;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

import org.joda.time.Interval;

public interface MammaBaseCapaciteitsBlokDao
{

	List<Object> getBlokTijden(List<Interval> nieuweBlokken, MammaCapaciteitBlok blok, Interval currentViewInterval);

	List<MammaCapaciteitBlok> getCapaciteitsBlokken(MammaScreeningsEenheid screeningEenheid, Date start, Date end);

	List<MammaCapaciteitBlok> getCapaciteitsBlokken(MammaScreeningsEenheid screeningEenheid, Date start, Date end, Collection<MammaCapaciteitBlokType> blokTypes);

	Long countCapaciteitsBlokken(MammaScreeningsEenheid screeningEenheid, Date start, Date end);

	Long countCapaciteitsBlokken(MammaScreeningsEenheid screeningEenheid, Date start, Date end, Collection<MammaCapaciteitBlokType> blokTypes);

	Collection<MammaCapaciteitBlokDto> getNietGeblokkerdeCapaciteitsBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date start, Date end,
		Collection<MammaCapaciteitBlokType> blokTypes);

}
