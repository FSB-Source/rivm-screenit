package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.repository.cervix.CervixBaseMonsterRepository;
import nl.rivm.screenit.service.cervix.CervixBepaalVervolgService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftOngeldigeHpvUitslag;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftOntvangstRonde;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftPap0CytologieUitslag;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftVervolgOnderzoekVoorOntvangstDatum;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.isMonsterType;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.isNietHetzelfdeMonster;

@Service
@AllArgsConstructor
public class CervixBepaalVervolgServiceImpl implements CervixBepaalVervolgService
{
	private final CervixBaseMonsterRepository monsterRepository;

	private <T extends CervixMonster> boolean ontvangenMonsters(CervixScreeningRonde ontvangstRonde, boolean ongeldig, boolean pap0, CervixMonster andersDan, Class<T> type)
	{
		var spec = Specification.where(heeftOntvangstRonde(ontvangstRonde).and(isMonsterType(type)));

		Specification<CervixMonster> disjunctionSpec = Specification.where(null);

		if (ongeldig)
		{
			disjunctionSpec = disjunctionSpec.or(heeftOngeldigeHpvUitslag());
		}

		if (pap0)
		{
			disjunctionSpec = disjunctionSpec.or(heeftPap0CytologieUitslag());
			spec = spec.and(heeftVervolgOnderzoekVoorOntvangstDatum());
		}

		spec = spec.and(disjunctionSpec);

		if (andersDan != null)
		{
			spec = spec.and(isNietHetzelfdeMonster(andersDan));
		}

		return monsterRepository.exists(spec);
	}

	@Override
	public boolean andereZasOngeldig(CervixZas zas)
	{
		return ontvangenMonsters(zas.getOntvangstScreeningRonde(), true, false, zas, CervixZas.class);
	}

	@Override
	public boolean anderUitstrijkjeOnbeoordeelbaar(CervixUitstrijkje uitstrijkje)
	{
		return ontvangenMonsters(uitstrijkje.getOntvangstScreeningRonde(), true, true, uitstrijkje, CervixUitstrijkje.class);
	}

	@Override
	public boolean anderUitstrijkjeOnbeoordeelbaarCytologie(CervixUitstrijkje uitstrijkje)
	{
		return ontvangenMonsters(uitstrijkje.getOntvangstScreeningRonde(), false, true, uitstrijkje, CervixUitstrijkje.class);
	}

	@Override
	public boolean uitstrijkjeOnbeoordeelbaarCytologie(CervixScreeningRonde ontvangstRonde)
	{
		return ontvangenMonsters(ontvangstRonde, false, true, null, CervixUitstrijkje.class);
	}
}
