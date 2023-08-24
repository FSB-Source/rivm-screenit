package nl.rivm.screenit.batch.jobs.colon.selectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
public class SelectieConstants
{

	public final static String RAPPORTAGEKEYSELECTIE = "key.colon.selectierapportage";

	public final static String COLONSELECTIEWAARSCHUWINGIFOBTS = "key.colon.selectie.ifobts.overschrijden.waarschuwing";

	public final static String COLONSELECTIEMAXIMAALIFOBTS = "key.colon.selectie.ifobts.overschrijden.maximaal";

	public final static String GEMEENTE_ZONDER_SCREENING_ORGANISATIES = "key.colon.selectie.gemeente.zonder.so";

	public static final String LAATSTE_RONDE_AANTAL_DAGEN_GELEDEN_GEMAAKT = "key.colon.selectie.laatste.ronde.aantal.dagen.geleden.gemaakt";

	public static final String PUSH_MAX_LEEFTIJD_COUNT = "key.colon.selectie.push.maxleeftijd.count";
}
