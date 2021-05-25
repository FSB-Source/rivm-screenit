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

import java.time.LocalDate;
import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.mamma.se.dto.actions.AfspraakMakenPassantDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakSignalerenDto;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;

public interface MammaAfspraakService
{
	void setAfspraakStatus(AfspraakSignalerenDto actionDto, MammaAfspraakStatus nieuweStatus, InstellingGebruiker gebruiker);

	Map<Long, Integer> getIngeschrevenByGebruikerOpDatumVoorSe(Date datum, String seCode);

	LocalDate getDatumVanOudsteNietAfgeslotenOnderzoek(String seCode);

	void afspraakMakenPassant(AfspraakMakenPassantDto actionDto, InstellingGebruiker gebruiker, MammaScreeningsEenheid screeningsEenheid);

    MammaAfspraak getOfMaakLaatsteAfspraakVanVandaag(Long afspraakId, InstellingGebruiker gebruiker);
}
