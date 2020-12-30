
package nl.rivm.screenit.batch.datasource;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.datasource.DataSourceRouter;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.MailService.MailPriority;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.hibernate.exception.LockAcquisitionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ReadOnlyDBActionsWithFallback
{

	private static final Logger LOG = LoggerFactory.getLogger(ReadOnlyDBActionsWithFallback.class);

	private ReadOnlyDBActionsWithFallback()
	{

	}

	public interface DelegatedReadOnlyDBActions
	{
		void doActions();
	}

	public static final void runReadOnlyDBActions(DelegatedReadOnlyDBActions delegated)
	{
		DataSourceRouter.useReadOnly();
		boolean foutInSlave = false;
		for (int counter = 0; counter < 4; counter++)
		{
			try
			{
				delegated.doActions();
				break;
			}
			catch (LockAcquisitionException lae)
			{
				foutInSlave = true;
				if (counter < 2)
				{
					LOG.error("Probleem op de slave met replicatie", lae);
				}
				else if (counter < 3)
				{
					LOG.error("Probleem op de slave met replicatie", lae);
					DataSourceRouter.useReadWrite();
				}
				else
				{
					LOG.error("Probleem op de master (laatste poging)", lae);
					throw new IllegalStateException("De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk.", lae);
				}
				try
				{
					Thread.sleep(15000);
				}
				catch (InterruptedException ie)
				{
					LOG.error("Fout in sleep", ie);
				}
			}
			catch (Exception e)
			{
				DataSourceRouter.useReadWrite();
				throw e;
			}
		}
		if (foutInSlave)
		{
			MailService mailService = SpringBeanProvider.getInstance().getBean(MailService.class);
			SimplePreferenceService preferenceService = SpringBeanProvider.getInstance().getBean(SimplePreferenceService.class);
			String email = preferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
			mailService.sendEmail(email, "DB slave is niet bereikbaar", 
				"Ter informatie: Na 3 pogingen is het niet gelukt om van een slave DB te lezen. " 
					+ "Er is automatisch overgeschakeld naar de Master DB. De batch applicatie heeft zijn werk daar kunnen voortzetten. " 
					+ "Er is geen actie vereisd nav. deze mail. " 
					+ "Als de mail niet naar Topicus is gestuurd, graag zsm. Topicus daarvan op de hoogte brengen.", 
				MailPriority.HIGH);
		}
		DataSourceRouter.useReadWrite();
	}
}
