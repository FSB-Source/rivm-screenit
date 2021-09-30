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

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class GebruikersnaamValidator extends BaseValidator<HuisartsDto>
{

	@Autowired
	private HuisartsRepository huisartsRepository;

	@Override
	public void validateTarget(HuisartsDto target, Errors errors)
	{
		Huisarts ingelogdeHuisarts = getIngelogdeHuisarts();
		if (target.getUsername() != null)
		{
			Huisarts gevondeHuisarts = huisartsRepository.findByGebruikersnaam(target.getUsername());
			if (gevondeHuisarts != null && !ingelogdeHuisarts.getHuisartsportaalId().equals(gevondeHuisarts.getHuisartsportaalId()))
			{
				errors.reject("error.username.gebruikt", "De gekozen gebruikersnaam wordt al gebruikt door een andere gebruiker.");
			}
		}
		else
		{
			errors.reject("error.username.leeg", "Gebruikersnaam is verplicht.");
		}
	}
}
