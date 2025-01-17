package nl.rivm.screenit.main.service.cervix;

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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.data.domain.Sort;

public interface CervixHuisartsService
{
	CervixHuisarts getHuisartsMetAgbCode(String agbCode);

	CervixHuisarts getUitstrijkendArtsMetAgb(String agbCode);

	CervixHuisarts maakOfWijzigUitstrijkendArts(CervixHuisarts uitstrijkendCervixHuisarts, InstellingGebruiker account) throws IllegalStateException;

	void saveCervixHuisartsLocatie(List<CervixHuisartsLocatie> locaties);

	void aanvraagLabformulieren(CervixLabformulierAanvraag labformulierAanvraag, CervixHuisartsLocatie huisartsLocatie, InstellingGebruiker instellingGebruiker);

	void saveOrUpdateArts(CervixHuisarts arts, LogGebeurtenis logGebeurtenis, InstellingGebruiker instellingGebruiker);

	CervixRegioBrief getLaatsteRegistratieBrief(CervixHuisarts arts);

	void saveOrUpdateLocatie(CervixHuisartsLocatie locatie);

	boolean isAndereLocatieMetNaam(CervixHuisartsLocatie locatie);

	void updateLabformulierAanvraag(CervixRegioMergedBrieven regioMergedBrieven);

	void resetWachtwoord(CervixHuisarts huisarts, Account loggedInAccount);

	void inactiveerHuisarts(CervixHuisarts huisarts, InstellingGebruiker loggedInAccount);

	List<CervixLabformulierAanvraag> getCervixLabformulierOrdersVanHuisarts(CervixHuisarts instelling, long first, long count, Sort sort);

	long getAantalCervixLabformulierOrdersVanHuisarts(CervixHuisarts instelling);

	List<CervixHuisartsLocatie> getActieveHuisartsLocatiesVanHuisarts(CervixHuisarts huisarts);

	List<CervixHuisartsLocatie> getLocatiesVanHuisarts(CervixHuisartsLocatie locatie, long first, long count, Sort sort);

	long getAantalLocatiesVanHuisarts(CervixHuisartsLocatie locatie);
}
