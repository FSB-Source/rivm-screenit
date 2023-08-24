package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.cervix.CervixHuisartsBericht_;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.CervixZas_;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixMonsterSpecification
{
	public static Specification<CervixMonster> heeftBrief()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(CervixMonster_.brief));
	}

	public static Specification<CervixMonster> heeftGeenVerrichtingen()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(CervixMonster_.verrichtingen));
	}

	public static Specification<CervixMonster> heeftGeenOntvangenHuisartsBericht()
	{
		return (r, q, cb) ->
		{
			var status = cb.treat(r, CervixUitstrijkje.class)
				.join(CervixUitstrijkje_.huisartsBericht, JoinType.LEFT)
				.get(CervixHuisartsBericht_.status);

			return cb.or(
				cb.isNull(status),
				cb.notEqual(status, CervixHuisartsBerichtStatus.AANGEMAAKT)
			);
		};
	}

	public static Specification<CervixMonster> isOntvangenUitstrijkje()
	{
		return (r, q, cb) ->
		{
			var status = cb.treat(r, CervixUitstrijkje.class).get(CervixUitstrijkje_.uitstrijkjeStatus);

			return cb.or(
				cb.isNull(status),
				cb.notEqual(status, CervixUitstrijkjeStatus.NIET_ONTVANGEN)
			);
		};
	}

	public static Specification<CervixMonster> isZas()
	{
		return (r, q, cb) -> cb.treat(r, CervixZas.class).get(CervixZas_.zasStatus).isNotNull();
	}
}
