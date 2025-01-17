package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.overig;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftBeeldenBeschikbaar;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isAangemaaktVoor;

@Component
@AllArgsConstructor
public class MammaBeeldenStatusOverigReader extends BaseSpecificationScrollableResultReader<MammaOnderzoek>
{

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MammaOnderzoek> createSpecification()
	{
		var peilMoment = currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN.name())).atStartOfDay();
		return isAangemaaktVoor(peilMoment).and(heeftBeeldenBeschikbaar());
	}
}
