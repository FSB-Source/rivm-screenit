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

import java.util.EnumSet;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.Errors;

@Component
public class HuisartsValidator extends BaseValidator<HuisartsDto>
{

	@Autowired
	private LocatieRepository locatieRepository;

	@Autowired
	private LocatieCriteriaRepository locatieCriteriaRepository;

	@Override
	public void validateTarget(HuisartsDto target, Errors errors)
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		List<Locatie> locaties = locatieCriteriaRepository.findByHuisartsAndStatussen(huisarts,
			EnumSet.of(CervixLocatieStatus.ACTIEF, CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD));
		if (CollectionUtils.isEmpty(locaties) && !huisarts.getAanmeldStatus().equals(AanmeldStatus.GEREGISTREERD))
		{
			errors.reject("error.locaties.min", "Minimaal 1 locatie nodig om de registratie af te ronden.");
		}

		String empty = "<empty>";
		if (!CollectionUtils.isEmpty(locaties))
		{
			for (Locatie locatie : locaties)
			{
				if (!locatie.getStatus().equals(CervixLocatieStatus.INACTIEF)
					&& (empty.equals(locatie.getIban()) || empty.equals(locatie.getNaam()) || empty.equals(locatie.getIbanTenaamstelling())))
				{
					errors.reject("error.locaties.bulk", "Er zijn nog lege velden bij een locatie, wijzig deze eerst.");
					break;
				}
			}
		}
	}
}
