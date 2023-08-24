package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixHpvOruBerichtenConstants
{
	public static final String CERVIX_HPV_ORU_BERICHT_VERSTUURD = "cervix.hpvoru.verstuurd";

	public static final String CERVIX_HPV_ORU_BERICHT_VERSTUURD_PER_LAB = "cervix.hpvoru.laboratorium.verstuurd";

	public static final String KEY_LABORATORIUMID = "cervix.laboratorium.id";

	public static final String CERVIX_HPV_ORU_BERICHT_VERSTUURDEN_TIMEOUT = "cervix.hpvoru.timeout";
}
