package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;

import nl.rivm.screenit.Constants;

public enum PlanningConstanten
{
	;

	public static int vanafLeeftijd;

	public static int totEnMetLeeftijd;

	public static final int STREEF_INTERVAL = 2;

	public static final int MAX_MELDINGEN_PER_SE = 10;

	public static int plannenVanafJaar;

	public static int plannenTotEnMetJaar;

	public static LocalDate plannenVanafDatum;

	public static LocalDate plannenTotEnMetDatum;

	public static LocalDate plannenVanafGeboortedatum;

	public static LocalDate plannenTotEnMetGeboortedatum;

	public static LocalDate prognoseVanafDatum;

	public static void set(LocalDate plannenVanafDatum, LocalDate plannenTotEnMetDatum, LocalDateTime nu, int vanafLeeftijd, int totEnMetLeeftijd)
	{
		PlanningConstanten.vanafLeeftijd = vanafLeeftijd;
		PlanningConstanten.totEnMetLeeftijd = totEnMetLeeftijd;

		PlanningConstanten.plannenVanafDatum = plannenVanafDatum.with(DayOfWeek.MONDAY);
		PlanningConstanten.plannenTotEnMetDatum = plannenTotEnMetDatum.with(DayOfWeek.SUNDAY);

		plannenVanafJaar = PlanningConstanten.plannenVanafDatum.getYear();
		plannenTotEnMetJaar = PlanningConstanten.plannenTotEnMetDatum.getYear();

		int plannenVanafGeboortejaar = plannenVanafJaar - PlanningConstanten.totEnMetLeeftijd;
		int plannenTotEnMetGeboortejaar = plannenTotEnMetJaar - PlanningConstanten.vanafLeeftijd;
		plannenVanafGeboortedatum = LocalDate.of(plannenVanafGeboortejaar, 1, 1);
		plannenTotEnMetGeboortedatum = LocalDate.of(plannenTotEnMetGeboortejaar, 12, 31);

		prognoseVanafDatum = nu.toLocalTime().isBefore(Constants.BK_EINDTIJD_DAG) ? nu.toLocalDate() : nu.toLocalDate().plusDays(1);
	}
}
