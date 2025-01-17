package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.net.InetAddress;
import java.net.UnknownHostException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.BuildDto;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.info.BuildProperties;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("build")
@Slf4j
public class BuildController
{

	@Autowired
	private BuildProperties buildProperties;

	@Value("${app.environment}")
	private String applicationEnvironment;

	@Value("${app.instance}")
	private String applicationInstance;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<BuildDto> build()
	{
		var build = new BuildDto();
		build.setVersion(buildProperties.getVersion());
		build.setTimestamp(buildProperties.getTime().toString());
		build.setInstance(getApplicationInstance());
		if (!applicationEnvironment.equalsIgnoreCase("PRODUCTIE"))
		{
			build.setEnvironment(applicationEnvironment);
		}
		return ResponseEntity.ok(build);
	}

	private String getApplicationInstance()
	{
		if (applicationInstance == null ||
			applicationInstance.isEmpty())
		{
			try
			{
				return InetAddress.getLocalHost().getHostName();
			}
			catch (UnknownHostException e)
			{
				LOG.error(e.getMessage(), e);
			}
		}
		return applicationInstance;
	}
}
