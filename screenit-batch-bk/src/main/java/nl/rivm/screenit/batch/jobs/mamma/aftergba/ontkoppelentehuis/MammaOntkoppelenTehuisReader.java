package nl.rivm.screenit.batch.jobs.mamma.aftergba.ontkoppelentehuis;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaTehuis;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.mamma.aftergba.AfterGbaJobConfiguration.AFTER_GBA_JOB_READER_FETCH_SIZE;
import static nl.rivm.screenit.specification.mamma.MammaTehuisSpecification.filterActief;

@Component
public class MammaOntkoppelenTehuisReader extends BaseSpecificationScrollableResultReader<MammaTehuis>
{
	public MammaOntkoppelenTehuisReader()
	{
		super.setFetchSize(AFTER_GBA_JOB_READER_FETCH_SIZE);
	}

	@Override
	protected Specification<MammaTehuis> createSpecification()
	{
		return filterActief(true);
	}
}
