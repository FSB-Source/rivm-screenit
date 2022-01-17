
package nl.rivm.screenit.batch.jobs.generalis.coordinaten.gemeentecoordstep;

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

import java.util.List;

import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.service.WoonplaatsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

public class GemeenteCoordinatenWriter implements ItemWriter<String>
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Autowired
	private WoonplaatsService woonplaatsService;

	@Override
	public void write(List<? extends String> items)
	{
		for (String item : items)
		{
			String[] lineParts = item.split(",");

			if (lineParts.length >= 10 && !lineParts[0].equals("pcnl_plaatsid"))
			{

				String plaatscode = lineParts[2];
				String gemcode = lineParts[3];
				String woonplaats = lineParts[4];
				String latitude = lineParts[8];
				String longitude = lineParts[9];
				coordinatenDao.addOrUpdateCoordinaten(gemcode, latitude, longitude);
				woonplaatsService.saveOrUpdateWoonplaats(plaatscode, woonplaats, gemcode);
			}
		}
		hibernateService.getHibernateSession().flush();
		hibernateService.getHibernateSession().clear();
	}
}
