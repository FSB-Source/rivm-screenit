package nl.rivm.screenit.main.service.mamma;

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

import java.io.File;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.BeoordelingenReserveringResult;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;

public interface MammaBeoordelingService
{
	List<MammaScreeningsEenheid> zoekScreeningsEenhedenMetBeWerklijstBeoordeling(InstellingGebruiker loggedInInstellingGebruiker,
		List<MammaBeoordelingStatus> beschikbarePaginaStatussen);

	List<MammaScreeningsEenheid> zoekScreeningsEenhedenMetCeWerklijstBeoordeling(List<MammaBeoordelingStatus> beschikbareBeoordelingStatussen,
		List<BeoordelingsEenheid> beoordelingsEenheden);

	List<MammaBeoordeling> zoekBeoordelingen(MammaBeWerklijstZoekObject zoekObject, Integer first, Integer count, String sortProperty, boolean asc);

	List<Long> zoekBeoordelingenNummers(MammaBeWerklijstZoekObject zoekObject, String sortProperty, boolean asc);

	long countOnderzoeken(MammaBeWerklijstZoekObject zoekObject);

	List<MammaBeoordeling> getAlleBeoordelingenMetBeelden(MammaBeoordeling beoordeling);

	List<MammaBeoordeling> getVorigeTweeTeTonenBeoordelingen(MammaBeoordeling beoordeling);

	int getAantalBeoordeeldInList(List<Long> beoordelingenIds);

	int getAantalBeoordeeld(MammaBeWerklijstZoekObject zoekObject);

	void bevestig1eEn2eLezingen(InstellingGebruiker instellingGebruiker);

	MammaLezing getOrCreate1eOf2eLezing(MammaBeoordeling beoordeling, InstellingGebruiker beoordelaar, boolean onervarenRadioloog);

	MammaLezing getOrCreateDiscrepantieOfArbitrageLezing(MammaBeoordeling beoordeling, MammaLezingType huidigeLezingType, InstellingGebruiker gebruiker);

	boolean is1eOf2eLezingenTeBevestigen(InstellingGebruiker instellingGebruiker);

	Long getVolgendeBeoordelingId(Long huidigeBeoordelingId, List<Long> beoordelingenIds);

	BeoordelingenReserveringResult openBeschikbareBeoordeling(Long startBeoordelingId, List<Long> beoordelingenIds, InstellingGebruiker ingelogdeGebruiker,
		MammaBeLezerSoort lezerSoort);

	void radioloogHeeftGeenHandtekening(Gebruiker medewerker);

	MammaLezing[] getLezingenVoorVerslag(MammaBeoordeling beoordeling);

	File verslagGoedkeurenDoorCE(MammaBeoordeling beoordeling, boolean directPrinten, EnovationHuisarts alternatieveHuisarts, InstellingGebruiker ingelogdeGebruiker);

	void onbeoordeelbaarAfgehandeld(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker);

	File genereerPdfVoorOngunstigeUitslagBrief(MammaBeoordeling beoordeling);

	void verslagAfkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker toegewezenRadioloog, InstellingGebruiker ingelogdeGebruiker);

	void verslagLaterGoedkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker);

	boolean isLezingValide(MammaLezing lezing, List<LaesieDto> laesieDtos);

	List<Object[]> beoordelingGeschiedenis(MammaBeoordeling beoordeling);

	void gunstigeUitslagMetNevenbevindingAfronden(MammaBeoordeling beoordeling, EnovationHuisarts huisarts, InstellingGebruiker loggedInInstellingGebruiker);

	boolean isBevoegdVoorArbitrage(InstellingGebruiker gebruiker);

	List<MammaBeoordelingOpschortenReden> getMogelijkeOpschortRedenen(MammaBeoordeling beoordeling, MammaLezingType lezingType);

	void logBeoordelingIngezien(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker, boolean isCoordinerendRadioloog);
}
