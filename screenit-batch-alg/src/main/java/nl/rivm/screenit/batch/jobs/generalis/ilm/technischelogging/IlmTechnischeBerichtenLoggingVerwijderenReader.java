package nl.rivm.screenit.batch.jobs.generalis.ilm.technischelogging;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.logging.TechnischeBerichtenLogRegel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.TechnischeBerichtenLogRegelSpecification.heeftRequestMomentVoor;

@Component
public class IlmTechnischeBerichtenLoggingVerwijderenReader extends BaseSpecificationScrollableResultReader<TechnischeBerichtenLogRegel>
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<TechnischeBerichtenLogRegel> createSpecification()
	{
		var peilMoment = currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN_NIET_MEDISCH.name()));
		return heeftRequestMomentVoor(peilMoment);
	}
}
