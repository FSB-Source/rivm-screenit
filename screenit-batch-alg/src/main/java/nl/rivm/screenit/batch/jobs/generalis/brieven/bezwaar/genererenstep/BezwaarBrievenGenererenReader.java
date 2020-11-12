package nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep;

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

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;

public class BezwaarBrievenGenererenReader extends AbstractBrievenGenererenReader<BezwaarBrief>
{

	@Override
	protected Long getScreeningOrganisatieId(ExecutionContext context)
	{
		return context.getLong(BezwaarBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Criteria additionalRestrictions(Criteria crit, ExecutionContext context)
	{
		crit.add(Restrictions.or(Restrictions.eq("client.gbaStatus", GbaStatus.INDICATIE_AANWEZIG), Restrictions.eq("client.gbaStatus", GbaStatus.BEZWAAR)));
		BriefType briefType = BriefType.valueOf(context.getString(BezwaarBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		crit.add(Restrictions.eq("briefType", briefType));
		return crit;
	}
}
