package nl.rivm.screenit.main.service.mamma;

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
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;

import org.springframework.data.domain.Sort;

public interface MammaVisitatieService
{
	List<MammaVisitatieOnderzoek> zoekVisitatieOnderzoeken(MammaVisitatieOnderdeel onderdeel, MammaVisitatie visitatie, long first, long count, Sort sort);

	long countVisitatieOnderzoeken(MammaVisitatieOnderdeel onderdeel, MammaVisitatie visitatie);

	List<MammaVisitatie> zoekVisitaties(LocalDate vanaf, List<MammaScreeningsEenheid> screeningsEenheden, List<MammaVisitatieStatus> statussen, long first, long count, Sort sort);

	long countVisitaties(LocalDate vanaf, List<MammaScreeningsEenheid> screeningsEenheden, List<MammaVisitatieStatus> statussen);

	boolean isBeoordelingInVisitatieOnderdeel(MammaBeoordeling beoordeling, MammaVisitatie visitatie, MammaVisitatieOnderdeel visitatieOnderdeel);

	long countAantalGezien(MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel);

	boolean isAllesGezien(MammaVisitatie visitatie);

	boolean kanVisitatieAfronden(MammaVisitatie visitatie);
}
