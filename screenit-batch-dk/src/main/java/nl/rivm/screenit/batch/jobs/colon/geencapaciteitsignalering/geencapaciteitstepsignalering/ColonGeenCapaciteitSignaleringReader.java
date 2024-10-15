package nl.rivm.screenit.batch.jobs.colon.geencapaciteitsignalering.geencapaciteitstepsignalering;

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
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.repository.colon.ColonIntakelocatieRepository;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.specification.colon.ColonIntakelocatieSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class ColonGeenCapaciteitSignaleringReader extends BaseSpecificationScrollableResultReader<ColonIntakelocatie, ColonIntakelocatieRepository>
{
	private final ColonIntakelocatieService intakelocatieService;

	@Override
	protected Specification<ColonIntakelocatie> createSpecification()
	{
		return ColonIntakelocatieSpecification.heeftGeenCapaciteitBinnenDatum(intakelocatieService.getSignaleringstermijnBereik()).and(ColonIntakelocatieSpecification.isActief());
	}
}
