package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.onafgerondeverwijzing;

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
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak_;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.colon.ColonConclusieSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftAfspraakVoor;

@Component
@AllArgsConstructor
public class OnbevestigdeIntakeVerwijzingenReader extends BaseSpecificationScrollableResultReader<ColonIntakeAfspraak>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Specification<ColonIntakeAfspraak> createSpecification()
	{
		return ColonConclusieSpecification.heeftType(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM).with(ColonIntakeAfspraak_.conclusie)
			.and(ColonConclusieSpecification.isDoorverwijzingBevestigd(false).with(ColonIntakeAfspraak_.conclusie))
			.and(heeftAfspraakVoor(currentDateSupplier.getLocalDateTime().plusDays(1)).with(ColonIntakeAfspraak_.nieuweAfspraak));
	}
}
