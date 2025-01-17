package nl.rivm.screenit.specification.cervix;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag_;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftGeenId;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixCytologieVerslagSpecification
{
	public static ExtendedSpecification<CervixCytologieVerslag> heeftGeenCytologieUitslag()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixCytologieVerslag_.cytologieUitslag));
	}

	public static ExtendedSpecification<CervixCytologieVerslag> heeftCytologieUitslag(CervixCytologieUitslag uitslag)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixCytologieVerslag_.cytologieUitslag), uitslag);
	}

	public static ExtendedSpecification<CervixCytologieVerslag> heeftGeenCytologieVerslag()
	{
		return heeftGeenId();
	}

}
