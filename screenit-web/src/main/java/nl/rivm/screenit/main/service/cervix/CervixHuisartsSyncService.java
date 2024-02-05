package nl.rivm.screenit.main.service.cervix;

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

import java.util.Date;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.ResetDto;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;

public interface CervixHuisartsSyncService
{

	void sendData(CervixHuisarts cervixHuisarts);

	void sendData(Overeenkomst overeenkomst);

	void sendData(CervixLabformulierAanvraag labformulierAanvraag);

	void sendData(CervixHuisartsLocatie locatie);

	void sendData(ResetDto resetDto);

	void updateHuisarts(HuisartsDto huisartsDto);

	String getPraktijkNaam(Gebruiker gebruiker);

	void nieuweAanvraagLabformulieren(AanvraagDto aanvraagDto);

	CervixHuisarts updateAndGetHuisarts(HuisartsDto huisartsDto, Date mutatieDatum);

	void setLabformulierAanvraag(AanvraagDto aanvraagDto);

	void updateLocatie(LocatieDto locatieDto);

	CervixHuisartsLocatieMutatieSoort getMutatieSoort(CervixHuisartsLocatie locatie, LocatieDto locatieDto);

}
