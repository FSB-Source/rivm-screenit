package nl.rivm.screenit.mamma.planning.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.concurrent.CountDownLatch;

import nl.rivm.screenit.PreferenceKey;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

public final class PlanningUitnodigingContext
{
	public final Integer minimaleIntervalMammografieOnderzoeken;

	public final Integer minimaleIntervalUitnodigingen;

	public final Integer afspraakBijUitnodigenVanafAantalWerkdagen;

	public final Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen;

	public final CountDownLatch onderbrokenCountDownLatch;

	public final CountDownLatch onderbrekenCountDownLatch;

	public boolean uitnodigenOnderbreken = false;

	public PlanningUitnodigingContext(SimplePreferenceService preferenceService, int aantalThreads)
	{
		minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
		minimaleIntervalUitnodigingen = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN.name());
		afspraakBijUitnodigenVanafAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN.toString());
		capaciteitVolledigBenutTotEnMetAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN.toString());

		onderbrekenCountDownLatch = new CountDownLatch(aantalThreads);
		onderbrokenCountDownLatch = new CountDownLatch(aantalThreads);
	}

}
