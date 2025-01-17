package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.ProjectSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.project.ProjectBriefActieType.HERINNERING;
import static nl.rivm.screenit.model.project.ProjectBriefActieType.XDAGENNAY;
import static nl.rivm.screenit.model.project.ProjectBriefActieType.XMETY;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.heeftDatum;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.heeftDatumVoorOfOp;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.heeftType;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.heeftTypeIn;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.isActief;

@Component
public class ProjectBrievenAanmaakReader extends BaseSpecificationScrollableResultReader<ProjectBriefActie>
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<ProjectBriefActie> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();
		return isActief(true)
			.and(projectIsActiefOp(vandaag))
			.and(magAangemaaktWordenOp(vandaag));
	}

	private Specification<ProjectBriefActie> projectIsActiefOp(LocalDate peilDatum)
	{
		return ProjectSpecification.isActiefOpDatum(peilDatum).with(ProjectBriefActie_.project);
	}

	private Specification<ProjectBriefActie> magAangemaaktWordenOp(LocalDate peilDatum)
	{
		return heeftTypeDatumEnMagAangemaaktWordenOp(peilDatum)
			.or(heeftTypeVanafDatumEnMagAangemaaktWordenOp(peilDatum))
			.or(heeftTypeIn(List.of(XDAGENNAY, XMETY, HERINNERING)));
	}

	private Specification<ProjectBriefActie> heeftTypeDatumEnMagAangemaaktWordenOp(LocalDate peilMoment)
	{
		return heeftType(ProjectBriefActieType.DATUM)
			.and(heeftDatum(peilMoment));
	}

	private Specification<ProjectBriefActie> heeftTypeVanafDatumEnMagAangemaaktWordenOp(LocalDate peilMoment)
	{
		return heeftType(ProjectBriefActieType.VANAF_DATUM)
			.and(heeftDatumVoorOfOp(peilMoment));
	}
}
