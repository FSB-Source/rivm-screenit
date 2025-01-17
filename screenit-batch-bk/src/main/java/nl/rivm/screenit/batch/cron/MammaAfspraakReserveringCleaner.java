package nl.rivm.screenit.batch.cron;

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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaAfspraakReserveringService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.util.DateUtil.formatShortDateTime;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;

@Slf4j
@Configuration
@EnableScheduling
@RequiredArgsConstructor
public class MammaAfspraakReserveringCleaner
{
	private final ICurrentDateSupplier dateSupplier;

	private final SimplePreferenceService preferenceService;

	private final MammaAfspraakReserveringService reserveringService;

	@Scheduled(cron = "0 */15 * * * *")
	@Transactional()
	public void verwijderOudeAfspraakReserveringen()
	{
		var nu = dateSupplier.getLocalDateTime();
		var reserveringenGeldigVoor = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_RESERVERING_GELDIG_VOOR.name(), 0);
		var peilMoment = nu.minusMinutes(reserveringenGeldigVoor);
		reserveringService.verwijderAfspraakReserveringenDieGemaaktZijnVoor(peilMoment);
		LOG.info("BK afspraakreserveringen die aangemaakt zijn voor {} zijn verwijderd", formatShortDateTime(toUtilDate(peilMoment)));
	}
}
