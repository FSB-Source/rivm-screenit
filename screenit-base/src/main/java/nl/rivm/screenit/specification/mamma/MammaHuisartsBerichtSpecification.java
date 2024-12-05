package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht_;
import nl.rivm.screenit.model.mamma.enums.MammaHuisartsBerichtStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaHuisartsBerichtSpecification
{
	public static ExtendedSpecification<MammaHuisartsBericht> heeftStatus(MammaHuisartsBerichtStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaHuisartsBericht_.status), status);
	}

	public static ExtendedSpecification<MammaHuisartsBericht> heeftStatusDatumOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaHuisartsBericht_.statusDatum), DateUtil.toUtilDate(peilMoment));
	}
}
