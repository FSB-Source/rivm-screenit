package nl.rivm.screenit.main.dao.cervix;

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

import java.util.List;

import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;

public interface CervixHuisartsDao
{

	CervixHuisarts getHuisarts(String agb);

	CervixRegioBrief getLaatsteRegistratieBrief(CervixHuisarts arts);

	Woonplaats getWoonplaats(String woonplaats);

	Woonplaats getWoonplaats(String woonplaats, String gemeenteNaam);

	List<CervixLabformulierAanvraag> getCervixLabformulierOrdersVanInstelling(CervixHuisarts instelling, long first, long count, SortState<String> sortState);

	long getAantalCervixLabformulierOrdersVanInstelling(CervixHuisarts instelling);

	List<CervixHuisartsLocatie> getActieveHuisartsLocatiesVanHuisarts(CervixHuisarts huisarts);

	List<CervixHuisartsLocatie> getCervixHuisartsLocatieVanHuisarts(CervixHuisartsLocatie locatie, long first, long count, SortState<String> sortState);

	long getAantalCervixHuisartsLocatieVanHuisarts(CervixHuisartsLocatie locatie);

	CervixHuisartsLocatie getCervixHuisartsLocatieWithName(CervixHuisartsLocatie locatie);

	List<CervixLabformulierAanvraag> getCervixLabformulierAanvraagFromMergedBrieven(CervixRegioMergedBrieven cervixRegioMergedBrieven);

	List<CervixLabformulierAanvraag> getNogNietVerstuurdeCervixLabformulierAanvraagVanLocatie(CervixHuisartsLocatie locatie);

}
