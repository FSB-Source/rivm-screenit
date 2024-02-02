package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.ScreeningOrganisatie_;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixVerrichtingSpecification
{

	public static Specification<CervixVerrichting> filterVerrichtingType(boolean huisartsVerichtingType, boolean labTariefTypes)
	{
		return (r, q, cb) ->
		{
			var values = new ArrayList<CervixTariefType>();
			if (huisartsVerichtingType && !labTariefTypes)
			{
				values.addAll(CervixTariefType.getAlleHuisartsTariefTypes());
			}
			else if (labTariefTypes && !huisartsVerichtingType)
			{
				values.addAll(CervixTariefType.getAlleLabTariefTypes());
			}
			else
			{
				values.addAll(List.of(CervixTariefType.values()));
			}
			return r.get(CervixVerrichting_.type).in(values);
		};
	}

	public static Specification<CervixVerrichting> filterVerrichtingDatumTotEnMet(LocalDate verrichtingDatum)
	{
		return skipWhenNull(verrichtingDatum, (r, q, cb) -> cb.lessThan(r.get(CervixVerrichting_.verrichtingsDatum), DateUtil.toUtilDate(verrichtingDatum.plusDays(1))));
	}

	public static Specification<CervixVerrichting> filterVerrichtingBinnenRegio(Long screeningOrganisatieId)
	{
		return skipWhenNull(screeningOrganisatieId, (r, q, cb) ->
		{
			var regioPath = join(r, CervixVerrichting_.regio);
			var regioId = regioPath.get(ScreeningOrganisatie_.id);

			return cb.equal(regioId, screeningOrganisatieId);
		});
	}
}
