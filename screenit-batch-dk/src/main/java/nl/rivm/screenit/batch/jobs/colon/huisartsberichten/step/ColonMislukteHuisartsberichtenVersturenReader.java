package nl.rivm.screenit.batch.jobs.colon.huisartsberichten.step;

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
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.colon.ColonHuisartsberichtSpecification.heeftStatus;
import static nl.rivm.screenit.specification.colon.ColonHuisartsberichtSpecification.isAangemaaktVanaf;

@Component
@AllArgsConstructor
public class ColonMislukteHuisartsberichtenVersturenReader extends BaseSpecificationScrollableResultReader<ColonHuisartsBericht>
{
	private final ICurrentDateSupplier currentDateSupplier;

	private static final Integer MAXIMALE_PERIODE_RETRY = 1;

	@Override
	protected Specification<ColonHuisartsBericht> createSpecification()
	{
		return heeftStatus(ColonHuisartsBerichtStatus.VERZENDEN_MISLUKT).and(isAangemaaktVanaf(currentDateSupplier.getLocalDateTime().minusMonths(MAXIMALE_PERIODE_RETRY)));
	}
}
