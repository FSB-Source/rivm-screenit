package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class IFOBTVerwerkingProcessor implements ItemProcessor<Long, IFOBTUitslag>
{
	private final HibernateService hibernateService;

	@Override
	public IFOBTUitslag process(Long item)
	{
		return hibernateService.load(IFOBTUitslag.class, item);
	}
}
