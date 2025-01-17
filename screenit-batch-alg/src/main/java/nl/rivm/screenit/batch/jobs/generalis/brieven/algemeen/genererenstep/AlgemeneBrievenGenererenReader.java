package nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.genererenstep;

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

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGbaStatus;

@Component
public class AlgemeneBrievenGenererenReader extends AbstractBrievenGenererenReader<AlgemeneBrief>
{
	@Override
	protected Long getScreeningOrganisatieId()
	{
		return null;
	}

	@Override
	protected Specification<AlgemeneBrief> createSpecification()
	{
		var specification = super.createSpecification();
		var briefType = BriefType.valueOf(getStepExecutionContext().getString(AlgemeneBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		return specification
			.and(heeftGbaStatus(GbaStatus.INDICATIE_AANWEZIG).with(r -> clientJoin(r)))
			.and(heeftBriefType(briefType));
	}
}
