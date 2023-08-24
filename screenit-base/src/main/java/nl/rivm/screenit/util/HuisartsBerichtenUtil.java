package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.HuisartsBericht;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

public class HuisartsBerichtenUtil
{
	public static boolean isLaatstVerstuurdeHuisartsbericht(HuisartsBericht huisartsBericht)
	{
		final HuisartsBericht deproxiedhuisartsBericht = (HuisartsBericht) HibernateHelper.deproxy(huisartsBericht);
		List<HuisartsBericht> huisartsBerichten = new ArrayList<>(deproxiedhuisartsBericht.getClient().getHuisartsBerichten()).stream()
			.map(HibernateHelper::deproxy)
			.filter(bericht -> bericht.getClass().equals(deproxiedhuisartsBericht.getClass()))
			.map(HuisartsBericht.class::cast)
			.collect(Collectors.toList());
		HuisartsBericht laatsteHuisartsbericht = Collections.max(huisartsBerichten, Comparator.comparing(HuisartsBericht::getAanmaakDatum));
		return deproxiedhuisartsBericht.equals(laatsteHuisartsbericht);
	}
}
