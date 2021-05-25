package nl.rivm.screenit.mamma.se.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.mamma.se.dto.MammaHuisartsDto;
import nl.rivm.screenit.mamma.se.dto.actions.InschrijvenDto;
import nl.rivm.screenit.mamma.se.dto.actions.SetEmailAdresDto;
import nl.rivm.screenit.model.InstellingGebruiker;

public interface InschrijvenService
{
	void inschrijven(InschrijvenDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd);

	void inschrijvingWijzigen(InschrijvenDto action, InstellingGebruiker instellingGebruiker);

	List<MammaHuisartsDto> getAllHuisartsen();

	void setEmailAdres(SetEmailAdresDto setEmailAdresDto, InstellingGebruiker ingelogdeGebruiker);
}
