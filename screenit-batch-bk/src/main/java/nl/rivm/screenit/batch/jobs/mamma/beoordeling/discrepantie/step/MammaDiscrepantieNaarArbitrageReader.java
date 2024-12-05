package nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie.step;

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
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.DISCREPANTIE;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.beoordelaarsZijnInactiefSinds;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatusDatumOpOfVoor;

@Component
public class MammaDiscrepantieNaarArbitrageReader extends BaseSpecificationScrollableResultReader<MammaBeoordeling>
{
	@Autowired
	protected ICurrentDateSupplier currentDateSupplier;

	private static final int MAX_UREN_IN_DISCREPANTIE = 24;

	private static final int BEOORDELAAR_INACTIEF_UREN = 2;

	@Override
	public Specification<MammaBeoordeling> createSpecification()
	{
		var beoordelaarsInactiefSindsPeilMoment = currentDateSupplier.getLocalDateTime().minusHours(BEOORDELAAR_INACTIEF_UREN);
		var startDatumOpOfVoor = currentDateSupplier.getLocalDateTime().minusHours(MAX_UREN_IN_DISCREPANTIE);

		return heeftStatus(DISCREPANTIE)
			.and(beoordelaarsZijnInactiefSinds(beoordelaarsInactiefSindsPeilMoment)
				.or(heeftStatusDatumOpOfVoor(startDatumOpOfVoor)));
	}
}
