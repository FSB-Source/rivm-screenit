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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

@AllArgsConstructor
public class MammaBriefSpecification
{
	public static PathAwarePredicate<MammaBrief> heeftBriefInBrieftypes(List<BriefType> briefTypes)
	{
		return (cb, r) -> r.get(Brief_.briefType).in(briefTypes);
	}

	public static PathAwarePredicate<MammaBrief> isGegenereerd(boolean isGegenereerd)
	{
		return (cb, r) -> cb.equal(r.get(Brief_.gegenereerd), isGegenereerd);
	}

	public static PathAwarePredicate<MammaBrief> heeftVervangendeProjectBrief(boolean heeftVervangendeProjectBrief)
	{
		return (cb, r) -> cb.equal(r.get(ClientBrief_.vervangendeProjectBrief), heeftVervangendeProjectBrief);
	}
}
