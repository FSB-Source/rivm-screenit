package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.huisartsenportaal.dto.BuildDto;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.info.BuildProperties;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class BuildController
{

	@Autowired
	private BuildProperties buildProperties;

	private static final Logger LOG = LoggerFactory.getLogger(BuildController.class);

	@Value("${applicationEnvironment}")
	private String applicationEnvironment;

	@Value("${applicationInstance}")
	private String applicationInstance;

	@RequestMapping("/build")
	public ResponseEntity build()
	{
		BuildDto build = new BuildDto();
		build.setVersion(buildProperties.getVersion());
		build.setTimestamp(buildProperties.getTime().toString());
		build.setInstance(getApplicationInstance());
		if (!applicationEnvironment.equalsIgnoreCase("PRODUCTIE"))
		{
			build.setEnvironment(applicationEnvironment);
		}
		return ResponseEntity.ok(build);
	}

	private String getApplicationInstance() {
		if (applicationInstance == null ||
			applicationInstance.isEmpty()) {
			try {
				return InetAddress.getLocalHost().getHostName();
			}
			catch (UnknownHostException e) {
				e.printStackTrace();
			}
		}
		return applicationInstance;
	}
}
