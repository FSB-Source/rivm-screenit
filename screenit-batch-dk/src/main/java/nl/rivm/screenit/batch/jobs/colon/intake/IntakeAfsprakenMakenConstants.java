package nl.rivm.screenit.batch.jobs.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
public class IntakeAfsprakenMakenConstants
{

	public static final String RAPPORTAGEKEYINTAKE = "key.intakemeldingid";

	public static final String AANTAL_EXTRA_DAGEN = "key.intakemaken.aantalextradagen";

	public static final String AANTAL_CLIENTEN = "key.intakemaken.aantalclienten";

	public static final String AANTAL_VRIJE_SLOTEN = "key.intakemaken.aantalvrijesloten";

	public static final String PLANNER_RESULTAAT = "key.intakemaken.plannerresultaat";

	public static final String HUIDIGE_RONDE = "key.intakemaken.huidigeronde";

	public static final String ALLE_INTAKES_VERWERKT = "key.intakemaken.alleintakesverwerkt";

	public static final String LAATSTE_EIND_DATUM = "key.intakemaken.laatsteeinddatum";

	public static final String FOUT_BIJ_INTAKE_VASTLEGGEN = "key.intakemaken.foutbijintakevastleggen";

}
