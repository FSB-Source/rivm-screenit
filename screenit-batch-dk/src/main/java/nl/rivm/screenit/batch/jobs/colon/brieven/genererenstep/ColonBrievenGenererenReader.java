package nl.rivm.screenit.batch.jobs.colon.brieven.genererenstep;

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

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
public class ColonBrievenGenererenReader extends AbstractBrievenGenererenReader<ColonBrief>
{

	@Override
	protected Long getScreeningOrganisatieId(ExecutionContext context)
	{
		return context.getLong(ColonBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Criteria additionalRestrictions(Criteria crit, ExecutionContext context)
	{
		crit.add(Restrictions.eq("client.gbaStatus", GbaStatus.INDICATIE_AANWEZIG));
		var briefType = BriefType.valueOf(context.getString(ColonBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		crit.add(Restrictions.eq("briefType", briefType));

		return crit;
	}
}
