package nl.rivm.screenit.batch.jobs.mamma.dense2;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
public class MammaDense2Constants
{
	public static final String AANTAL_CLIENTEN_IN_EXPORT_EERSTE_STUDIERONDE = "mamma.aantal_clienten_in_export_eerste_studieronde";

	public static final String AANTAL_CLIENTEN_IN_EXPORT_TWEEDE_STUDIERONDE = "mamma.aantal_clienten_in_export_tweede_studieronde";

	public static final String AANTAL_ONDERZOEKEN_DENSITEIT_VERWIJDERD = "mamma.aantal_onderzoeken_densiteit_verwijderd";

}
