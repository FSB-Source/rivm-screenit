package nl.rivm.screenit.mamma.se.proxy;

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

import java.io.IOException;
import java.io.InputStream;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.TimeZone;

import nl.rivm.screenit.mamma.se.proxy.model.EnvironmentInfoDto;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.event.ContextRefreshedEvent;

@SpringBootApplication
public class SeProxyApplication implements ApplicationListener<ContextRefreshedEvent>
{
	private static final Logger LOG = LoggerFactory.getLogger(SeProxyApplication.class);

	private static EnvironmentInfoDto environmentInfo = null; 

	private static String environmentName;

	@Autowired
	private SeRestSocketService seRestSocketService;

	@Autowired
	private SeStatusService seStatusService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private AchtergrondRequestService achtergrondRequestService;

	@Autowired
	private MammaScreeningsEenheidStatusService statusService;

	private boolean initPerformed = false;

	public static void main(String[] args)
	{
		new SpringApplicationBuilder()
			.sources(SeProxyApplication.class)
			.run(args);
	}

	@Bean(name = "databasePath")
	public String databasePath(@Value("${spring.datasource.url}") String path)
	{
		return path;
	}

	public static EnvironmentInfoDto getEnvironmentInfo()
	{
		if (environmentInfo == null)
		{
			environmentInfo = new EnvironmentInfoDto();
			var applicationProperties = new Properties();
			try (InputStream resourceAsStream = SeProxyApplication.class.getResourceAsStream("/build-info.properties"))
			{
				applicationProperties.load(resourceAsStream);
				environmentInfo.setVersion(applicationProperties.getProperty("build.version"));
				LOG.info("SE-Proxy versie: " + environmentInfo.getVersion());
				environmentInfo.setEnvironment(environmentName);
				environmentInfo.setMagUpdaten(false);
				String timestampString = applicationProperties.getProperty("build.time").replace('T', ' ').replace('Z', ' ').trim();
				SimpleDateFormat inputformat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
				inputformat.setTimeZone(TimeZone.getTimeZone("UTC"));
				Date timestamp = inputformat.parse(timestampString, new ParsePosition(0));
				if (timestamp == null)
				{
					timestamp = new Date();
				}
				SimpleDateFormat outputformat = new SimpleDateFormat("dd-MM-yyyy HH:mm");
				outputformat.setTimeZone(TimeZone.getTimeZone("CET"));
				environmentInfo.setTimestamp(outputformat.format(timestamp));
			}
			catch (IOException e)
			{
				LOG.error("Fout bij laden van build-info.properties (voor environmentInfo)");
			}
		}
		return environmentInfo;
	}

	@Value("${ENVIRONMENT:Productie}")
	public void setEnvironmentName(String environment)
	{
		environmentName = environment;
	}

	@Override
	public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent)
	{
		if (!initPerformed)
		{
			initPerformed = true;
			LOG.info("Spring context ready: init SE-code");
			seStatusService.initSeCode();
			LOG.info("Spring context ready: init proxyservice");
			proxyService.init();
			LOG.info("Spring context ready: start achtergrondophaler");
			achtergrondRequestService.ensureRunning();
			LOG.info("Spring context ready: start websocketverbinding met centraal");
			seRestSocketService.initRestSocketService();
			LOG.info("Spring context ready: stuur status naar centraal");
			statusService.maakStatusEnQueueRequestNaarCentraal();
		}
		else
		{
			LOG.info("OnApplicationEvent called when init already performed. Do nothing");
		}
	}
}
