package nl.rivm.screenit.huisartsenportaal.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordVergetenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;

public interface HuisartsService
{

	Huisarts updateHuisarts(HuisartsDto huisartsDto, Huisarts huisarts);

	Huisarts setHuisarts(HuisartsDto huisartsDto);

	Huisarts updatePassword(Huisarts huisarts, String wachtwoord);

	boolean controleerPassword(String rawPassword, String encodedPassword);

	Huisarts wachtwoordVergeten(Huisarts huisarts) throws IllegalStateException;

	Huisarts getHuisartsWith(WachtwoordVergetenDto dto);

	Huisarts getHuisartsWith(Long ScreenitId);

	Integer increaseAttemps(Huisarts huisarts);

	void resetAttemps(Huisarts huisarts);

	Long remainingMinutes(Huisarts huisarts);

}
