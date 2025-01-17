package nl.rivm.screenit.batch.jobs.brieven.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.time.LocalDateTime;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.algemeen.AlgemeneMergedBrieven_;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.util.DateUtil.toUtilDate;

public abstract class AbstractBrievenCleanUpReader<MB extends MergedBrieven<?>> extends BaseSpecificationScrollableResultReader<MB>
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MB> createSpecification()
	{
		var peilMoment = currentDateSupplier.getLocalDateTime().minusDays(getBewaarTermijnInDagen());

		return isVerwijderd(false)
			.and(heeftGeenBrieven()
				.or(heeftPrintDatumOpOfVoor(peilMoment)));
	}

	private Specification<MB> heeftPrintDatumOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.and(
			cb.isTrue(r.get(MergedBrieven_.geprint)),
			cb.lessThanOrEqualTo(r.get(MergedBrieven_.printDatum), toUtilDate(peilMoment)));
	}

	private Specification<MB> heeftGeenBrieven()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(AlgemeneMergedBrieven_.BRIEVEN));
	}

	protected Specification<MB> isVerwijderd(Boolean verwijderd)
	{
		return (r, q, cb) -> cb.equal(r.get(MergedBrieven_.verwijderd), verwijderd);
	}

	protected int getBewaarTermijnInDagen()
	{
		return 14;
	}
}
