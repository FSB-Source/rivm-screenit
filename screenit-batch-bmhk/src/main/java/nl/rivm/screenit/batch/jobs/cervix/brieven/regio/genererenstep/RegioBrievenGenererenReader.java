package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.BriefType;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
public class RegioBrievenGenererenReader extends AbstractBrievenGenererenReader<CervixRegioBrief>
{

	@Override
	protected Long getScreeningOrganisatieId(ExecutionContext context)
	{
		return context.getLong(RegioBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var stepContext = getStepExecutionContext();
		var crit = session.createCriteria(CervixRegioBrief.class);
		crit.createAlias("regio", "regio");
		crit.add(Restrictions.eq("regio.id", getScreeningOrganisatieId(stepContext)));

		crit.add(Restrictions.eq("gegenereerd", false));
		crit.add(Restrictions.eq("vervangen", false));
		crit.add(Restrictions.eq("tegenhouden", false));

		crit.add(Restrictions.eq("briefType", BriefType.REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS));

		return crit;
	}
}
