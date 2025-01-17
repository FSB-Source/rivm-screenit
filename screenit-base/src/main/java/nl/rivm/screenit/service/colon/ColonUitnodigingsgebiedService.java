package nl.rivm.screenit.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.colon.CapaciteitsPercWijziging;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;

public interface ColonUitnodigingsgebiedService
{
	List<PostcodeGebied> findOverlappendePostcodeGebieden(PostcodeGebied postcode);

	List<CapaciteitsPercWijziging> bepaalCapaciteitsWijzigingen(UitnodigingsGebied uitnodigingsGebied, Map<String, Integer> newAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems);

	BigDecimal getFitFactorVoorGebied(UitnodigingsGebied uitnodigingsGebied);

	void wijzigingenDoorvoeren(UitnodigingsGebied uitnodiginsgebied, Map<String, Integer> newAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems, List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen, InstellingGebruiker ingelogdeGebruiker);

	List<UitnodigingsGebied> getAllUitnodigingsgebieden();

	String valideerAdherentieVanGewijzigdeGebieden(Set<UitnodigingsGebied> gewijzigdeGebieden);

	void wijzigingenDoorvoeren(ColonIntakelocatie intakelocatie, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems,
		List<CapaciteitsPercWijziging> capaciteitsPercWijzigingen, InstellingGebruiker loggedInInstellingGebruiker);

	List<CapaciteitsPercWijziging> bepaalCapaciteitsWijzigingen(ColonIntakelocatie intakelocatie, Map<String, Integer> newAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeItems);

	long countPersonenInUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd,
		LocalDate laatsteDagVanHuidigJaar, Set<Integer> geboortejaren);

	long countPersonenInUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied);

	long countClientenInUitnodigingsgebiedMetUitnodigingOpDatum(UitnodigingsGebied uitnodigingsGebied, LocalDate uitnodigingsDatum);

	String getUniekIdOf(ColoscopieCentrumColonCapaciteitVerdeling verdeling);
}
