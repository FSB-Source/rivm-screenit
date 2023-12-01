package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.mamma.se.proxy.model.CacheProxyActie;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.CleanUpService;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.GebruikerStoreService;
import nl.rivm.screenit.mamma.se.proxy.services.KwaliteitsopnameStoreService;
import nl.rivm.screenit.mamma.se.proxy.services.NfcOtpAdministratieService;
import nl.rivm.screenit.mamma.se.proxy.services.PersistableTransactionService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CleanUpServiceImpl implements CleanUpService
{
	@Autowired
	private NfcOtpAdministratieService nfcOtpAdministratieService;

	@Autowired
	private SeDaglijstService seDaglijstService;

	@Autowired
	private PersistableTransactionService persistableTransactionService;

	@Autowired
	private WerklijstStoreService werklijstStoreService;

	@Autowired
	private GebruikerStoreService gebruikerStoreService;

	@Autowired
	private KwaliteitsopnameStoreService kwaliteitsopnameStoreService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private ConfiguratieService configuratieService;

	@Autowired
	private AchtergrondRequestService achtergrondRequestService;

	private final Object cleanupLock = new Object();

	private LocalDate laatsteUpdateDag;

	@Override
	public void startOfDayCleanup()
	{
		kwaliteitsopnameStoreService.resetVolgnrs();
		werklijstStoreService.startOfDayCleanUp();
	}

	@Override
	public void startOfDayCleanupUitvoerenIndienNodig()
	{
		synchronized (cleanupLock)
		{
			LocalDate vandaag = DateUtil.getCurrentDateTime().toLocalDate();
			if (laatsteUpdateDag == null || vandaag.isAfter(laatsteUpdateDag))
			{
				laatsteUpdateDag = vandaag;
				startOfDayCleanup();
			}
		}
	}

	@Override
	public void nightlyCleanupAndRefresh()
	{
		clearOldEntries();
		refreshCaches(CacheProxyActie.ALTIJD_OPHALEN_ALS_ONLINE);
	}

	@Override
	public void updateCachesAtOnlineEvent()
	{
		refreshCaches(CacheProxyActie.OPHALEN_ALS_NIET_GECACHT);
	}

	private void clearOldEntries()
	{
		nfcOtpAdministratieService.clearOldEntries();
		gebruikerStoreService.clearOldEntries();
		persistableTransactionService.clearOldEntries();
		proxyService.deleteOudePlanningCache();
	}

	private void refreshCaches(CacheProxyActie cacheProxyActie)
	{
		achtergrondRequestService.queueStamdataVerversRequests(cacheProxyActie);
		verversGeenScreeningPlanningBlokken(cacheProxyActie);
		seDaglijstService.haalDaglijstenOp();
	}

	private void verversGeenScreeningPlanningBlokken(CacheProxyActie cacheProxyActie)
	{
		LocalDate vandaag = DateUtil.getCurrentDateTime().toLocalDate();

		Integer daglijstOphalenVoorDagen = configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_DAGLIJST_OPHALEN_VOOR_DAGEN);
		for (int i = 0; i <= daglijstOphalenVoorDagen; i++)
		{
			achtergrondRequestService.queueVerversGeenScreeningBlokken(vandaag.plusDays(i), cacheProxyActie);
		}
	}
}
