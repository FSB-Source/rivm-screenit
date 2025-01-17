package nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.JobStartParameter;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.heeftStatus;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftGeenCytologieOrder;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftLaboratoriumMetId;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftNietStatusIn;

@Component
public class CervixOrderAanmaakReader extends BaseSpecificationScrollableResultReader<CervixUitstrijkje>
{

	@Override
	public Specification<CervixUitstrijkje> createSpecification()
	{
		var specification = heeftNietStatusIn(List.of(CervixUitstrijkjeStatus.NIET_ONTVANGEN, CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR))
			.and(heeftStatus(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE).with(CervixUitstrijkje_.labformulier))
			.and(heeftGeenCytologieOrder());

		var jobParameters = getStepExecution().getJobExecution().getJobParameters();
		if (jobParameters.toProperties().containsKey(JobStartParameter.CERVIX_ORDER_LABORATORIUM.name()))
		{
			specification = specification.and(heeftLaboratoriumMetId(jobParameters.getLong(JobStartParameter.CERVIX_ORDER_LABORATORIUM.name())));
		}
		return specification;
	}
}
