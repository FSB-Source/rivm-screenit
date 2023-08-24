package nl.rivm.screenit.main.dao.mamma;

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

import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaAdhocMeekijkverzoekWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieWerklijstZoekObject;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;

public interface MammaKwaliteitscontroleDao
{

	List<MammaFotobespreking> zoekFotobesprekingen(MammaFotobesprekingWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc);

	long countFotobesprekingen(MammaFotobesprekingWerklijstZoekObject zoekObject);

	List<MammaFotobesprekingOnderzoek> zoekFotobesprekingOnderzoeken(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject, int first, int count, String sortProperty,
		boolean ascending);

	long countFotobesprekingOnderzoeken(MammaFotobesprekingOnderzoekenWerklijstZoekObject zoekObject);

	Integer getAantalBesproken(MammaFotobespreking fotobespreking);

	boolean isAllesBesproken(MammaFotobespreking fotobespreking);

	boolean isBeoordelingInBespreking(MammaBeoordeling beoordeling, MammaFotobespreking fotobespreking);

	List<MammaVisitatieOnderzoek> zoekVisitatieOnderzoeken(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending);

	long countVisitatieOnderzoeken(MammaVisitatieOnderzoekenWerklijstZoekObject zoekObject);

	List<MammaVisitatie> zoekVisitaties(MammaVisitatieWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc);

	long countVisitaties(MammaVisitatieWerklijstZoekObject zoekObject);

	Integer getAantalGezien(MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel);

	boolean isAllesGezien(MammaVisitatie visitatie);

	boolean isBeoordelingInVisitatieOnderdeel(MammaBeoordeling beoordeling, MammaVisitatie visitatie, MammaVisitatieOnderdeel visitatieOnderdeel);

	boolean nieuweBeoordelingenAangevraagdNavFotobespreking(MammaFotobespreking fotobespreking);

	List<MammaAdhocMeekijkverzoek> zoekAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject, int first, int count, SortState<String> sortState);

	long countAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject);

	Integer getAantalGezienAdhocMeekijkverzoekOnderzoeken(MammaAdhocMeekijkverzoekWerklijstZoekObject zoekObject);

	Integer getAantalGezienAdhocMeekijkverzoekOnderzoekenInList(List<Long> onderzoekenIds);

}
