package nl.rivm.screenit.specification.colon;

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

import java.util.Date;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.colon.SKMLExternSchema_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonSKMLExternSchemaSpecification
{
	public static ExtendedSpecification<SKMLExternSchema> filterRonde(Integer ronde)
	{
		return skipWhenNullExtended(ronde, (r, q, cb) -> cb.equal(r.get(SKMLExternSchema_.ronde), ronde));
	}

	public static ExtendedSpecification<SKMLExternSchema> filterLetter(String letter)
	{
		return skipWhenEmptyExtended(letter, (r, q, cb) -> cb.equal(r.get(SKMLExternSchema_.letter), letter));
	}

	public static ExtendedSpecification<SKMLExternSchema> filterJaar(Integer jaar)
	{
		return skipWhenNullExtended(jaar, (r, q, cb) -> cb.equal(r.get(SKMLExternSchema_.jaar), jaar));
	}

	public static ExtendedSpecification<SKMLExternSchema> filterDeadline(Date deadline)
	{
		return skipWhenNullExtended(deadline, (r, q, cb) -> cb.equal(r.get(SKMLExternSchema_.deadline), deadline));
	}

	public static ExtendedSpecification<SKMLExternSchema> heeftDeadlineVanaf(Date deadline)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(SKMLExternSchema_.deadline), DateUtil.startDag(deadline));
	}

	public static ExtendedSpecification<SKMLExternSchema> filterActief(Boolean actief)
	{
		return skipWhenNullExtended(actief, (r, q, cb) -> cb.equal(r.get(SKMLExternSchema_.actief), actief));
	}
}
