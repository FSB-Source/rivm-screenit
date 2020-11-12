
package nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class PostcodeCoordinatenIntakeLocatieKoppelWriter extends BaseWriter<ColoscopieCentrum>
{
	
	private static final Logger LOGGER = LoggerFactory.getLogger(PostcodeCoordinatenClientKoppelWriter.class);

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Override
	public void write(ColoscopieCentrum item)
	{
		PostcodeCoordinaten coordinaten = null;
		for (Adres adres : item.getAdressen())
		{
			if (coordinaten == null)
			{
				coordinaten = coordinatenDao.getCoordinaten(adres);
				if (coordinaten != null)
				{
					break;
				}
				else
				{
					LOGGER.warn("Geen coordinaten gevonden voor IL " + item.getNaam() + " " + AdresUtil.getVolledigeAdresString(adres));
				}
			}
		}
		item.setPostcodeCoordinaten(coordinaten);
		if (coordinaten != null)
		{
			LOGGER.info("Coordinaten voor IL " + item.getNaam() + " gevonden.");
		}
		getHibernateService().saveOrUpdate(item);
	}
}
