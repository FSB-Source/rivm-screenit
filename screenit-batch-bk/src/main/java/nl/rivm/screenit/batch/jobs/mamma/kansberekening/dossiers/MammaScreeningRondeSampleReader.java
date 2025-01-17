package nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers;

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

import java.time.LocalDate;

import javax.persistence.criteria.From;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaBrief_;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isAangemaaktVoor;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isGegenereerd;
import static nl.rivm.screenit.specification.algemeen.MammaBriefSpecification.heeftGeenMergedBrieven;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.heeftPrintDatumVoor;
import static nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification.heeftPrintDatum;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isAangemaaktIn;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftGeenScreeningRondeEvent;

@Component
@AllArgsConstructor
public class MammaScreeningRondeSampleReader extends BaseSpecificationScrollableResultReader<MammaScreeningRonde>
{
	private final ICurrentDateSupplier dateSupplier;

	@Override
	protected Specification<MammaScreeningRonde> createSpecification()
	{
		var vandaag = dateSupplier.getLocalDate();
		var aangemaaktBereik = Range.atLeast(vandaag.minusYears(5));
		var printDatumVoor = vandaag.minusWeeks(Constants.DEELNAMEKANSBEREKENING_NA_WEKEN);

		return heeftGeenScreeningRondeEvent()
			.and(isAangemaaktIn(aangemaaktBereik))
			.and(gegenereerdeBriefZonderMergedBrievenAangemaaktVoor(printDatumVoor)
				.or(briefMetMergedBrievenGeprintVoor(printDatumVoor)));
	}

	private ExtendedSpecification<MammaScreeningRonde> gegenereerdeBriefZonderMergedBrievenAangemaaktVoor(LocalDate peilmoment)
	{
		return heeftGeenMergedBrieven()
			.and(isGegenereerd(true))
			.and(isAangemaaktVoor(peilmoment))
			.with(r -> getBriefJoin(r));
	}

	private ExtendedSpecification<MammaScreeningRonde> briefMetMergedBrievenGeprintVoor(LocalDate peilmoment)
	{
		return heeftPrintDatum()
			.and(heeftPrintDatumVoor(peilmoment))
			.with(r -> getMergedBrievenJoin(r));
	}

	private static From<?, MammaBrief> getBriefJoin(From<?, ? extends MammaScreeningRonde> r)
	{
		var uitnodigingJoin = join(r, MammaScreeningRonde_.uitnodigingen);
		return join(uitnodigingJoin, MammaUitnodiging_.brief);
	}

	private static From<?, MammaMergedBrieven> getMergedBrievenJoin(From<?, ? extends MammaScreeningRonde> r)
	{
		var uitnodigingJoin = join(r, MammaScreeningRonde_.uitnodigingen);
		var briefJoin = join(uitnodigingJoin, MammaUitnodiging_.brief);
		return join(briefJoin, MammaBrief_.mergedBrieven, LEFT);
	}
}
