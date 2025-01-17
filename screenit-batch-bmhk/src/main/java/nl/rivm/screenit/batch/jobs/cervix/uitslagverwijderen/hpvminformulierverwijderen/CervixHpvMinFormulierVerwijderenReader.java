package nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.hpvminformulierverwijderen;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulier_;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde.NEGATIEF;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isAfgerond;
import static nl.rivm.screenit.specification.cervix.CervixHpvBeoordelingSpecification.heeftHpvUitslag;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierSpecification.isNietGewist;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftBrief;
import static nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification.isAangemeld;

@Component
public class CervixHpvMinFormulierVerwijderenReader extends BaseSpecificationScrollableResultReader<CervixLabformulier>
{

	@Override
	protected Specification<CervixLabformulier> createSpecification()
	{
		return (r, q, cb) ->
		{
			var uitstrijkjeJoin = join(r, CervixLabformulier_.uitstrijkje);
			var ontvangstScreeningRondeJoin = join(uitstrijkjeJoin, CervixMonster_.ontvangstScreeningRonde);
			var monsterHpvUitslagJoin = join(ontvangstScreeningRondeJoin, CervixScreeningRonde_.monsterHpvUitslag);
			var laatsteHpvBeoordelingJoin = join(monsterHpvUitslagJoin, CervixMonster_.laatsteHpvBeoordeling);
			return heeftHpvUitslag(NEGATIEF).with(root -> laatsteHpvBeoordelingJoin)
				.and(isAfgerond().with(root -> ontvangstScreeningRondeJoin))
				.and(isAangemeld(true).with(root -> ontvangstScreeningRondeJoin))
				.and(isNietGewist().with(root -> r))
				.and(heeftBrief().with(root -> uitstrijkjeJoin)).toPredicate(r, q, cb);
		};
	}
}
