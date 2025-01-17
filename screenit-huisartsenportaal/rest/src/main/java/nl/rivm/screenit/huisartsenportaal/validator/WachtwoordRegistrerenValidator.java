package nl.rivm.screenit.huisartsenportaal.validator;

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

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class WachtwoordRegistrerenValidator extends BaseWachtwoordValidator<HuisartsDto>
{

	@Override
	public void validateTarget(HuisartsDto target, Errors errors)
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		if (StringUtils.isNotBlank(huisarts.getPassword()))
		{

			return;
		}

		if (target.getWachtwoord() == null)
		{
			errors.reject("error.oudepassword.null", "Het wachtwoord dient ingevuld te zijn.");
		}
		else
		{
			controleerGebruikersnaamInWachtwoord(target.getUsername(), target.getWachtwoord(), errors);
			controleerTekenEisen(target.getWachtwoord(), errors);
		}
	}
}
