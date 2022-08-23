package nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.util.AdresUtil;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class PostcodeCoordinatenStandplaatsKoppelWriter extends BaseWriter<MammaStandplaatsLocatie>
{

	private final CoordinatenDao coordinatenDao;

	@Override
	public void write(MammaStandplaatsLocatie standplaatsLocatie)
	{
		PostcodeCoordinaten coordinaten = coordinatenDao.getCoordinaten(standplaatsLocatie);
		standplaatsLocatie.setPostcodeCoordinaten(coordinaten);
		if (coordinaten != null)
		{
			LOG.info("Coordinaten voor adres " + standplaatsLocatie.getId() + " gevonden.");
		}
		else
		{
			LOG.warn("Geen coordinaten gevonden voor adres " + standplaatsLocatie.getId() + " " + AdresUtil.getVolledigeAdresString(standplaatsLocatie));
		}
		getHibernateService().saveOrUpdate(standplaatsLocatie);
	}
}
