package nl.rivm.screenit.batch.jobs.colon.selectie.projectinterval;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftMeerdereProjectenOfHeeftParameterKey;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftType;

@Component
@AllArgsConstructor
public class ColonProjectIntervalToepassenReader extends BaseSpecificationScrollableResultReader<ProjectClient>
{

	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<ProjectClient> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();

		return heeftType(ProjectType.PROJECT).with(ProjectClient_.project).and(heeftMeerdereProjectenOfHeeftParameterKey(vandaag));
	}
}
