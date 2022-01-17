package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class SeStatusServiceImpl implements SeStatusService
{

	@Autowired
	private ConfiguratieService configuratieService;

	@Value("${SE_CODE:#{null}}")
	private String seCode;

	private boolean online;

	private boolean testOnline = true;

	private String proxyIp;

	private static final String PROXY_ADDRESS_POST_FIX = ".50";

	@Override
	public void initSeCode()
	{
		if (seCode == null)
		{
			seCode = configuratieService.getConfiguratieValue(SeConfiguratieKey.SE_CODE);
		}
	}

	@Override
	public String getProxyIp()
	{
		return proxyIp;
	}

	@Override
	public void setProxyIpFromClientIpIfSeCodeIsMissing(String clientIp)
	{
		if (seCode == null)
		{
			this.proxyIp = maakProxyIp(clientIp);
		}
	}

	@Override
	public void setSeCode(String seCode)
	{
		this.seCode = seCode;
		configuratieService.insertOrUpdateConfiguratieValue(SeConfiguratieKey.SE_CODE, seCode);
	}

	@Override
	public String getSeCode()
	{
		return seCode;
	}

	@Override
	public void setVerbindingStatus(boolean online)
	{
		this.online = online;
	}

	@Override
	public boolean isOnline()
	{
		return online;
	}

	@Override
	public boolean isTestOnline()
	{
		return testOnline;
	}

	@Override
	public void setTestOnline(boolean testOnline)
	{
		this.testOnline = testOnline;
	}

	private String maakProxyIp(String ip)
	{
		int indexLastDot = ip.lastIndexOf('.');
		if (indexLastDot == -1)
		{
			throw new IllegalStateException("Zet SE_CODE, u werkt waarschijnlijk via localhost.");
		}
		String newIp = ip.substring(0, indexLastDot);
		return newIp + PROXY_ADDRESS_POST_FIX;
	}
}
