package nl.rivm.screenit.wsb.fhir.interceptor;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.inject.Inject;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Slf4j
@Configuration
@EnableScheduling
public class FQDNProvider
{

	private static final String JOB_OMSCHRIJVING = "Reset de FQDNs vanuit de parameterisatie";

	@Inject
	private SimplePreferenceService simplePreferenceService;

	private List<String> validFQDNs = new ArrayList<>();

	private ThreadLocal<String> currentFQDN = new ThreadLocal<>();

	@Scheduled(cron = "0 */5 * * * ?")
	public void removeVerlopenSessies()
	{
		OpenHibernate5Session.withoutTransaction().run(() ->
		{
			LOG.debug("Start: " + JOB_OMSCHRIJVING);
			String validFQDNstring = simplePreferenceService.getString(PreferenceKey.INTERNAL_CERVIX_LAB_FORMULIER_VALID_FQDNS.name());
			if (StringUtils.isNotBlank(validFQDNstring))
			{
				validFQDNs = Arrays.asList(validFQDNstring.split(";"));
			}
			else
			{
				validFQDNs = new ArrayList<>();
			}
			LOG.debug("Stop: " + JOB_OMSCHRIJVING);
		});
	}

	boolean isFQDNCheckNeeded()
	{
		return !validFQDNs.isEmpty();
	}

	boolean isFQDNValid(String checkFQDN)
	{
		return validFQDNs.contains(checkFQDN);
	}

	public void registerFQDN(String fqdn)
	{
		currentFQDN.set(fqdn);
	}

	public String getCurrentFQDN()
	{
		return currentFQDN.get();
	}

	public void clearCurrentFQDN()
	{
		currentFQDN.set(null);
	}
}
