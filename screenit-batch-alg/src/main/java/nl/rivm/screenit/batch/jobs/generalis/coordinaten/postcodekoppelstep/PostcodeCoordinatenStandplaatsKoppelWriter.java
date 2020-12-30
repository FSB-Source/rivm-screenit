
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

import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.util.AdresUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

public class PostcodeCoordinatenStandplaatsKoppelWriter extends BaseWriter<MammaStandplaatsLocatie>
{

	private static final Logger LOGGER = LoggerFactory.getLogger(PostcodeCoordinatenClientKoppelWriter.class);

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Override
	public void write(MammaStandplaatsLocatie standplaatsLocatie)
	{
		PostcodeCoordinaten coordinaten = coordinatenDao.getCoordinaten(standplaatsLocatie);
		standplaatsLocatie.setPostcodeCoordinaten(coordinaten);
		if (coordinaten != null)
		{
			LOGGER.info("Coordinaten voor adres " + standplaatsLocatie.getId() + " gevonden.");
		}
		else
		{
			LOGGER.warn("Geen coordinaten gevonden voor adres " + standplaatsLocatie.getId() + " " + AdresUtil.getVolledigeAdresString(standplaatsLocatie));
		}
		getHibernateService().saveOrUpdate(standplaatsLocatie);
	}
}
