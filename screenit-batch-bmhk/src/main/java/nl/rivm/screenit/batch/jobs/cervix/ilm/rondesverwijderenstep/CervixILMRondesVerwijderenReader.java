package nl.rivm.screenit.batch.jobs.cervix.ilm.rondesverwijderenstep;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftStatus;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.heeftStatusDatumVoorOfOp;

@Component
public class CervixILMRondesVerwijderenReader extends BaseSpecificationScrollableResultReader<CervixScreeningRonde>
{

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier dateSupplier;

	public CervixILMRondesVerwijderenReader(SimplePreferenceService preferenceService, ICurrentDateSupplier dateSupplier)
	{
		super.setFetchSize(50);
		this.preferenceService = preferenceService;
		this.dateSupplier = dateSupplier;
	}

	@Override
	protected Specification<CervixScreeningRonde> createSpecification()
	{
		var minVerwijderenDatum = DateUtil.toUtilDate(dateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN.name())));

		return heeftStatus(ScreeningRondeStatus.AFGEROND)
			.and(heeftStatusDatumVoorOfOp(minVerwijderenDatum));
	}
}
