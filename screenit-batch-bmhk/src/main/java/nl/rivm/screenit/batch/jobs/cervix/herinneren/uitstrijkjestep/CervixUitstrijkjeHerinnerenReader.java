package nl.rivm.screenit.batch.jobs.cervix.herinneren.uitstrijkjestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps.CervixHerinnerenReader;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixBrief_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.cervix.CervixUitnodigingSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
public class CervixUitstrijkjeHerinnerenReader extends CervixHerinnerenReader
{

	private static final int CERVIX_UITSTRIJKJE_HERINNEREN_READER_FETCH_SIZE = 50;

	public CervixUitstrijkjeHerinnerenReader()
	{
		super(CERVIX_UITSTRIJKJE_HERINNEREN_READER_FETCH_SIZE,
			OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_UITSTRIJKJE,
			CervixMonsterType.UITSTRIJKJE);
	}

	@Override
	protected Specification<CervixUitnodiging> createSpecification()
	{
		var maxPeriodeDatum = getMaxPeriodeDatum(PreferenceKey.CERVIX_HERINNERINGS_PERIODE_NON_RESPONDER);
		return super.createSpecification()
			.and(CervixUitnodigingSpecification.heeftMergedBrievenVoorDatum(maxPeriodeDatum));
	}

	@Override
	protected Order getOrder(Root<CervixUitnodiging> r, CriteriaBuilder cb)
	{
		if (getMaxAantalHerinneringen(OrganisatieParameterKey.CERVIX_MAX_AANTAL_HERINNERINGEN_UITSTRIJKJE) != null)
		{
			var brief = SpecificationUtil.join(r, CervixUitnodiging_.brief);
			return cb.asc(SpecificationUtil.join(brief, CervixBrief_.mergedBrieven).get(MergedBrieven_.printDatum));
		}
		return null;
	}
}
