package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.jms.Session;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.MammaAfgebrokenDownloadOnderzoekCleanupService;
import nl.rivm.screenit.batch.service.MammaVerzamelDownloadOnderzoekDataService;

import org.apache.activemq.command.ActiveMQTextMessage;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.SUPPORTS)
@Slf4j
@Component
@RequiredArgsConstructor
public class JMSVerzamelOnderzoekDataBerichtListener implements SessionAwareMessageListener<ActiveMQTextMessage>
{
	private final MammaVerzamelDownloadOnderzoekDataService verzamelDownloadOnderzoekDataService;

	private final MammaAfgebrokenDownloadOnderzoekCleanupService cleanupService;

	private boolean cleanupServiceGedraaid = false;

	@Override
	public void onMessage(ActiveMQTextMessage message, Session session)
	{
		LOG.info("Verzamelen van onderzoekensdata is getriggerd.");
		try
		{
			draaiCleanupServiceBijEersteAanroep();

			verzamelDownloadOnderzoekDataService.getAlleVerzamelDownloadOnderzoekDataVerzoeken().forEach(verzamelDownloadOnderzoekDataService::verzamelOnderzoekData);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij afwerken van downloadverzoeken", e);
		}
	}

	private void draaiCleanupServiceBijEersteAanroep()
	{
		if (!cleanupServiceGedraaid)
		{
			cleanupService.checkAfgebrokenDownloadOnderzoekenVerzoekEnRuimOp();
			cleanupServiceGedraaid = true;
		}
	}

}
