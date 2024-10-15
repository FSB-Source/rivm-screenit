package nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class PostcodeCoordinatenIntakeLocatieKoppelWriter extends BaseWriter<ColonIntakelocatie>
{

	private final CoordinatenService coordinatenService;

	@Override
	public void write(ColonIntakelocatie item)
	{
		PostcodeCoordinaten coordinaten = null;
		for (Adres adres : item.getAdressen())
		{
			if (coordinaten == null)
			{
				coordinaten = coordinatenService.getCoordinaten(adres);
				if (coordinaten != null)
				{
					break;
				}
				else
				{
					LOG.warn("Geen coordinaten gevonden voor IL " + item.getNaam() + " " + AdresUtil.getVolledigeAdresString(adres));
				}
			}
		}
		item.setPostcodeCoordinaten(coordinaten);
		if (coordinaten != null)
		{
			LOG.info("Coordinaten voor IL " + item.getNaam() + " gevonden.");
		}
		getHibernateService().saveOrUpdate(item);
	}
}
