package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.model.EnvironmentInfoDto;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.PersistableTransactionService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/environmentInfo")
public class EnvironmentInfoProxyController
{
	@Value("${DISABLE_NFC_AUTHENTICATION:#{false}}")
	private boolean disableNFCAuthentication;

	@Autowired
	private LogischeSessieService logischeSessieService;

	@Autowired
	private PersistableTransactionService persistableTransactionService;

	@Autowired
	private SeDaglijstService seDaglijstService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private ConfiguratieService configuratieService;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<EnvironmentInfoDto> readBuildinfo(HttpSession httpSession, HttpServletRequest request)
	{
		EnvironmentInfoDto environmentInfo = new EnvironmentInfoDto(SeProxyApplication.getEnvironmentInfo());
		environmentInfo.setNfcEnabled(String.valueOf(!disableNFCAuthentication));
		environmentInfo.setHuidigWerkstationIpAdres(request.getRemoteAddr());
		environmentInfo.setMagUpdaten(!logischeSessieService.zijnErNietVerlopenSessies() && !persistableTransactionService.zijnErWachtendeTransacties());
		environmentInfo.setDagenInDaglijstCache(seDaglijstService.dagenInCache());
		environmentInfo.setCacheVulling(proxyService.cacheVullingInfo());
		environmentInfo.setDagenDaglijstOphalenLimiet(configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_DAGLIJST_OPHALEN_VOOR_DAGEN));
		environmentInfo.setTomosyntheseMogelijk(configuratieService.getConfiguratieBooleanValue(SeConfiguratieKey.TOMOSYNTHESE_MOGELIJK));
		return ResponseEntity.ok(environmentInfo);
	}
}
