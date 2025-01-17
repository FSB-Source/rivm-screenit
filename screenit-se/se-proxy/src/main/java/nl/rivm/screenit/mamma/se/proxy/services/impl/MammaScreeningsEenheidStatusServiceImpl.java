package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.model.EnvironmentInfoDto;
import nl.rivm.screenit.mamma.se.proxy.model.SeStatusDto;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MammaScreeningsEenheidStatusServiceImpl implements MammaScreeningsEenheidStatusService
{

	@Autowired
	private AchtergrondRequestService achtergrondRequestService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private SeDaglijstService daglijstService;

	@Override
	public void maakStatusEnQueueRequestNaarCentraal()
	{
		EnvironmentInfoDto environmentInfo = SeProxyApplication.getEnvironmentInfo();
		proxyService.cacheVullingInfo();
		SeStatusDto statusDto = new SeStatusDto();
		statusDto.setVersie(environmentInfo.getVersion());
		statusDto.setHuisartsenAanwezig(proxyService.huisartsenInCache());
		statusDto.setZorginstellingenAanwezig(proxyService.zorginstellingeninCache());
		statusDto.setMammografenAanwezig(proxyService.mammografenInCache());
		statusDto.setOfflineDaglijsten(daglijstService.dagenInCache());
		statusDto.setStatusMoment(LocalDateTime.now());
		achtergrondRequestService.queueStatusPostenRequest(statusDto);
		achtergrondRequestService.ensureRunning();
	}
}
