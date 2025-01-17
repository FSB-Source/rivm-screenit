package nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken;

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

import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus.BEEINDIGD;
import static nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus.GEPLAND;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftStatusIn;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.valtInDatumTijdPeriode;
import static nl.rivm.screenit.specification.mamma.MammaKansberekeningAfspraakEventSpecification.heeftGeenOpkomst;

@Component
@AllArgsConstructor
public class MammaAfspraakSampleReader extends BaseSpecificationScrollableResultReader<MammaAfspraak>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MammaAfspraak> createSpecification()
	{
		var peilMomentVanaf = currentDateSupplier.getLocalDate().minusYears(5).atStartOfDay();
		var peilMomentTot = currentDateSupplier.getLocalDateTime();

		return heeftStatusIn(List.of(GEPLAND, BEEINDIGD))
			.and(valtInDatumTijdPeriode(Range.closedOpen(peilMomentVanaf, peilMomentTot)))
			.and(heeftGeenOpkomst().with(MammaAfspraak_.afspraakEvent, JoinType.LEFT));
	}
}
