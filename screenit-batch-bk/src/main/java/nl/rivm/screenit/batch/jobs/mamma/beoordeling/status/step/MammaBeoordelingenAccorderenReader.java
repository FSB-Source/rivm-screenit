package nl.rivm.screenit.batch.jobs.mamma.beoordeling.status.step;

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

import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.mamma.MammaLezingSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatus;

@Component
public class MammaBeoordelingenAccorderenReader extends BaseSpecificationScrollableResultReader<MammaBeoordeling>
{
	@Autowired
	protected ICurrentDateSupplier currentDateSupplier;

	private static final int MAX_UREN_NIET_GEACCORDEERD = 24;

	private static final int BEOORDELAAR_INACTIEF_UREN = 2;

	@Override
	public Specification<MammaBeoordeling> createSpecification()
	{
		var beoordelaarsInactiefSindsPeilMoment = currentDateSupplier.getLocalDateTime().minusHours(BEOORDELAAR_INACTIEF_UREN);
		var maxUrenNietGeaccordeerdPeilMoment = currentDateSupplier.getLocalDateTime().minusHours(MAX_UREN_NIET_GEACCORDEERD);

		var isEersteLezingOpgeslagenEnEersteBeoordelaarInactief = heeftStatus(EERSTE_LEZING_OPGESLAGEN)
			.and(MammaLezingSpecification.heeftNietBeoordeeldSindsSubquery(beoordelaarsInactiefSindsPeilMoment).with(MammaBeoordeling_.eersteLezing, JoinType.LEFT));

		var isTweedeLezingOpgeslagenEnTweedeBeoordelaarInactief = heeftStatus(TWEEDE_LEZING_OPGESLAGEN)
			.and(MammaLezingSpecification.heeftNietBeoordeeldSindsSubquery(beoordelaarsInactiefSindsPeilMoment).with(MammaBeoordeling_.tweedeLezing, JoinType.LEFT));

		var isEersteLezingOpgeslagenEnDatumBuitenTermijn = heeftStatus(EERSTE_LEZING_OPGESLAGEN)
			.and(MammaLezingSpecification.heeftBeoordelingDatumOpOfVoor(maxUrenNietGeaccordeerdPeilMoment).with(MammaBeoordeling_.eersteLezing, JoinType.LEFT));

		var isTweedeLezingOpgeslagenEnDatumBuitenTermijn = heeftStatus(TWEEDE_LEZING_OPGESLAGEN)
			.and(MammaLezingSpecification.heeftBeoordelingDatumOpOfVoor(maxUrenNietGeaccordeerdPeilMoment).with(MammaBeoordeling_.tweedeLezing, JoinType.LEFT));

		return isEersteLezingOpgeslagenEnEersteBeoordelaarInactief
			.or(isTweedeLezingOpgeslagenEnTweedeBeoordelaarInactief)
			.or(isEersteLezingOpgeslagenEnDatumBuitenTermijn)
			.or(isTweedeLezingOpgeslagenEnDatumBuitenTermijn);
	}

}
