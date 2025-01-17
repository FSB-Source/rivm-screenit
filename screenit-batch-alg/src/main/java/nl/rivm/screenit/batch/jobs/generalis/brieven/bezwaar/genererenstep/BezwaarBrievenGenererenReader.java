package nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep;

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

import java.util.List;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGbaStatusIn;

@Component
public class BezwaarBrievenGenererenReader extends AbstractBrievenGenererenReader<BezwaarBrief>
{
	@Override
	protected Long getScreeningOrganisatieId()
	{
		return getStepExecutionContext().getLong(BezwaarBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Specification<BezwaarBrief> createSpecification()
	{
		var specification = super.createSpecification();
		var briefType = BriefType.valueOf(getStepExecutionContext().getString(BezwaarBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		return specification
			.and(heeftGbaStatusIn(List.of(GbaStatus.INDICATIE_AANWEZIG, GbaStatus.BEZWAAR)).with(r -> clientJoin(r)))
			.and(heeftBriefType(briefType));
	}
}
