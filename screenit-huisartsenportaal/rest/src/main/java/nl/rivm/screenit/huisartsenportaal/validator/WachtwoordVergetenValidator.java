package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordVergetenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class WachtwoordVergetenValidator extends BaseValidator<WachtwoordVergetenDto>
{
	@Autowired
	private HuisartsService huisartsService;

	@Override
	public void validateTarget(WachtwoordVergetenDto dto, Errors errors)
	{
		if (dto.getGebruikersnaam() == null || dto.getEmail() == null)
		{
			errors.reject("error.field.null", "Vul een gebruikersnaam en e-mail in om opnieuw uw wachtwoord op te vragen.");
		}

		Huisarts huisarts = huisartsService.getHuisartsWith(dto);
		if (huisarts == null || !huisarts.isEnabled())
		{
			errors.reject("error.invalid.huisarts", "Huisarts niet gevonden met deze gebruikersnaam en e-mail");
		}
	}
}
