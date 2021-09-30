package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordWijzigenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class WachtwoordWijzigenValidator extends BaseValidator<WachtwoordWijzigenDto>
{
	private static String passwordRegex = "^(?=.*?[A-Z])(?=(.*[a-z]){1,})(?=(.*[\\d]){1,})(?=(.*[\\W]){1,})(?!.*\\s).{8,}$";

	@Autowired
	private HuisartsService huisartsService;

	@Override
	public void validateTarget(WachtwoordWijzigenDto target, Errors errors)
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		if (target.getOudeWachtwoord() == null && huisarts.getInlogCode() == null)
		{
			errors.reject("error.oudepassword.null", "Het huidige wachtwoord dient ingevuld te zijn.");
		}
		if (huisarts != null && target.getNieuweWachtwoord() != null && target.getNieuweWachtwoordControle() != null
			&& (target.getOudeWachtwoord() != null || huisarts.getInlogCode() != null) && InlogMethode.USERNAME_PASSWORD.equals(huisarts.getInlogMethode()))
		{

			String encodedPassword = huisarts.getPassword();
			if (huisarts.getInlogCode() == null && !huisartsService.controleerPassword(target.getOudeWachtwoord(), encodedPassword))
			{
				errors.reject("error.oudepassword", "Huidige wachtwoord is verkeerd.");
			}

			if (!target.getNieuweWachtwoord().equals(target.getNieuweWachtwoordControle()))
			{
				errors.reject("error.password.equals", "De twee nieuwe wachtwoorden zijn niet gelijk.");
			}

			if (!target.getNieuweWachtwoord().matches(passwordRegex))
			{
				errors.reject("error.password.not.valid", "Het nieuwe wachtwoord voldoet niet aan de gestelde eisen.");
			}
		}
	}
}
