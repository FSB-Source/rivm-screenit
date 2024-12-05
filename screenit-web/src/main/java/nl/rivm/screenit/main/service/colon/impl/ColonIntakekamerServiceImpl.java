
package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.dao.LocatieDao;
import nl.rivm.screenit.main.service.colon.ColonIntakekamerService;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer_;
import nl.rivm.screenit.repository.colon.ColonIntakekamerRepository;
import nl.rivm.screenit.specification.colon.ColonIntakeKamerSpecification;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
public class ColonIntakekamerServiceImpl implements ColonIntakekamerService
{
	@Autowired
	private LocatieDao locatieDao;

	@Autowired
	private ColonIntakekamerRepository kamerRepository;

	@Override
	public List<ColonIntakekamer> getKamers(ColonIntakelocatie intakelocatie)
	{
		return kamerRepository.findAll(ColonIntakeKamerSpecification.isActief().and(ColonIntakeKamerSpecification.heeftIntakelocatie(intakelocatie)),
			Sort.by(Sort.Order.asc(ColonIntakekamer_.ID)));
	}

	@Override
	public List<ColonIntakekamer> getKamers(int first, int count, SortParam<String> sort)
	{
		return locatieDao.getKamers(first, count, sort);
	}

}
