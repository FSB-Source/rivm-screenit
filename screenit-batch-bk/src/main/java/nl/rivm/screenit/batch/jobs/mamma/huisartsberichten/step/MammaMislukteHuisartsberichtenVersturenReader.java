package nl.rivm.screenit.batch.jobs.mamma.huisartsberichten.step;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.enums.MammaHuisartsBerichtStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.mamma.MammaHuisartsBerichtSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaHuisartsBerichtSpecification.heeftStatusDatumOpOfVoor;

@Component
@AllArgsConstructor
public class MammaMislukteHuisartsberichtenVersturenReader extends BaseSpecificationScrollableResultReader<MammaHuisartsBericht>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MammaHuisartsBericht> createSpecification()
	{
		var huisartsBerichtWachtTijd = currentDateSupplier.getLocalDateTime().minusMinutes(Constants.BK_HA_BATCH_BERICHTEN_AANMAAK_OUDER_DAN);
		return heeftStatus(MammaHuisartsBerichtStatus.VERSTUREN_MISLUKT).or(
			heeftStatus(MammaHuisartsBerichtStatus.AANGEMAAKT).and(heeftStatusDatumOpOfVoor(huisartsBerichtWachtTijd)));
	}
}
