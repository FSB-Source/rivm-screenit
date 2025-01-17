package nl.rivm.screenit.huisartsenportaal.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvraagStatistiekenDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvraagTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.TableResultOptionsDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;

public interface LabformulierService
{

	AanvraagTotalenDto getAanvragenHuisarts(Huisarts huisarts, TableResultOptionsDto tableResultOptionsDto);

	LabformulierAanvraag saveScreenITAanvraag(AanvraagDto aanvraagDto);

	AanvraagStatistiekenDto getAanvraagStatistiekenLocatie(Long locatiePortaalId);

	AanvraagDto convertToDto(LabformulierAanvraag aanvraag);

	void verwijderNogNietVerstuurdeLabformulierenVanLocatie(Locatie locatie);

}
