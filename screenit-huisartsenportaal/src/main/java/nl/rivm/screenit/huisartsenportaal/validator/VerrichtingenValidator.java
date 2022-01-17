package nl.rivm.screenit.huisartsenportaal.validator;

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

import java.util.Date;

import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingFilterDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingZoekObjectDto;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class VerrichtingenValidator extends BaseValidator<VerrichtingZoekObjectDto>
{
	@Override
	public void validateTarget(VerrichtingZoekObjectDto target, Errors errors)
	{
		VerrichtingFilterDto verrichtingFilterDto = target.getVerrichtingenZoekObject();
		if (verrichtingFilterDto != null)
		{
			Date vanafDatum = verrichtingFilterDto.getVerrichtingsDatumVanaf();
			Date totenmetDatum = verrichtingFilterDto.getVerrichtingsDatumTotenmet();
			if (vanafDatum != null && totenmetDatum != null)
			{
				int compare = vanafDatum.compareTo(totenmetDatum);
				if (compare > 0)
				{
					errors.reject("error.verrichting.geldigetotenmetdatum", "De tot en met datum moet gelijk zijn of na de vanaf datum liggen.");
				}
			}
		}
	}
}
