package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.concurrent.atomic.AtomicReference;

import javax.jms.Session;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.MammaCStoreService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.activemq.command.ActiveMQTextMessage;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.SUPPORTS)
@Component
@Slf4j
@AllArgsConstructor
public class JMSUploadBeeldenBerichtListener implements SessionAwareMessageListener<ActiveMQTextMessage>
{
	private final MammaCStoreService uploadBeeldenService;

	private final SimplePreferenceService preferenceService;

	@Override
	public void onMessage(ActiveMQTextMessage message, Session session)
	{
		LOG.info("Uploaden van beelden is getriggerd.");

		AtomicReference<String> sopClasses = new AtomicReference<>();
		OpenHibernate5Session.withoutTransaction().run(() ->
			sopClasses.set(preferenceService.getString(PreferenceKey.MAMMA_DICOM_SOP_CONFIG.name()))
		);

		uploadBeeldenService.getOpenstaandeUploadVerzoeken().forEach(uploadBeeldenVerzoek -> uploadBeeldenService.verstuurBeelden(uploadBeeldenVerzoek, sopClasses.get()));
	}
}
