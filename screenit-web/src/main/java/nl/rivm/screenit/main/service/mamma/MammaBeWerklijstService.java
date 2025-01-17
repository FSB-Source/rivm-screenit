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

import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;

import org.springframework.data.domain.Sort;

public interface MammaBeWerklijstService
{
	boolean heeftOnderzoekenInWerklijst(InstellingGebruiker loggedInInstellingGebruiker, BeoordelingsEenheid beoordelingsEenheid);

	List<MammaBeoordeling> zoekBeoordelingen(MammaBeWerklijstZoekObject zoekObject, long first, long count, Sort sort);

	long countBeoordelingen(MammaBeWerklijstZoekObject zoekObject);

	List<Long> zoekBeoordelingenNummers(MammaBeWerklijstZoekObject zoekObject, Sort sort);

	List<MammaScreeningsEenheid> zoekScreeningsEenhedenMetBeWerklijstBeoordeling(InstellingGebruiker loggedInInstellingGebruiker,
		List<MammaBeoordelingStatus> beschikbarePaginaStatussen);

	int getAantalBeoordeeld(MammaBeWerklijstZoekObject zoekObject);

	int getAantalBeoordeeldInList(List<Long> beoordelingenIds);

	boolean is1eOf2eLezingenTeBevestigen(InstellingGebruiker instellingGebruiker);

	void bevestig1eEn2eLezingen(InstellingGebruiker instellingGebruiker);

}
