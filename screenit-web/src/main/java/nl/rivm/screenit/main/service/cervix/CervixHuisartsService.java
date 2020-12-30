package nl.rivm.screenit.main.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.joda.time.DateTime;

public interface CervixHuisartsService
{

	CervixHuisarts getUitstrijkendArtsMetAgb(String agbCode);

	public CervixHuisarts maakOfWijzigUitstrijkendArts(CervixHuisarts uitstrijkendCervixHuisarts, InstellingGebruiker account) throws IllegalStateException;

	public void saveCervixHuisartsLocatie(List<CervixHuisartsLocatie> locaties);

	public void sendRegistratieMail(CervixHuisarts huisarts);

	void aanvraagLabformulieren(CervixLabformulierAanvraag labformulierAanvraag, CervixHuisartsLocatie huisartsLocatie, InstellingGebruiker instellingGebruiker);

	void saveOrUpdateArts(CervixHuisarts arts, LogGebeurtenis logGebeurtenis, InstellingGebruiker instellingGebruiker);

	Date getMutatiedatumUitstrijkendArts(CervixHuisarts cervixHuisarts);

	List<CervixHuisarts> getUitstrijkendArtsen(int first, int count, String orderByProperty, boolean ascending, String agbCode, DateTime mutatiedatumVanaf,
		DateTime mutatiedatumTot, Gemeente... gemeentes);

	long countUitstrijkendArts(String agbCode, DateTime mutatiedatumVanaf, DateTime mutatiedatumTot, Gemeente... gemeentes);

	CervixRegioBrief getLaatsteRegistratieBrief(CervixHuisarts arts);

	void saveOrUpdateLocatie(CervixHuisartsLocatie locatie);

	boolean isAndereLocatieMetNaam(CervixHuisartsLocatie locatie);

	void updateLabformulierAanvraag(CervixRegioMergedBrieven regioMergedBrieven);

	void resetWachtwoord(CervixHuisarts huisarts, Account loggedInAccount);

	void inactiveerHuisarts(CervixHuisarts huisarts, InstellingGebruiker loggedInAccount);

}
