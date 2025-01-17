package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaKansberekeningConstants
{

	public static final String STANDPLAATS_RONDE_DEELNAME_GEMIDDELDEN_KEY = "standplaats.ronde.deelname.gemiddelden";

	public static final String REGIO_DEELNAME_GEMIDDELDEN_KEY = "regio.deelname.gemiddelden";

	public static final String STANDPLAATS_RONDE_OPKOMST_GEMIDDELDEN_KEY = "standplaats.ronde.opkomst.gemiddelden";

	public static final String REGIO_OPKOMST_GEMIDDELDEN__KEY = "regio.opkomst.gemiddelden";

	public static final String SCREENING_RONDE_SAMPLES_KEY = "screening.ronde.samples";

	public static final String SCREENING_RONDE_EVENTS_KEY = "screening.ronde.events";

	public static final String AFSPRAAK_SAMPLES_KEY = "afspraak.samples";

	public static final String AFSPRAAK_EVENTS_KEY = "afspraak.events";
}
