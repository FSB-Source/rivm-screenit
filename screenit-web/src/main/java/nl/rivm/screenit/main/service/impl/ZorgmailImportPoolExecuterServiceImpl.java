package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.FileInputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.main.service.ZorgmailImportPoolExecuterService;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ZorgmailImportService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ZorgmailImportPoolExecuterServiceImpl implements ZorgmailImportPoolExecuterService
{

	private static final Logger LOG = LoggerFactory.getLogger(ZorgmailImportPoolExecuterServiceImpl.class);

	private ExecutorService executorService;

	@Autowired
	private ZorgmailImportService importService;

	@Autowired
	private LogService logService;

	public ZorgmailImportPoolExecuterServiceImpl()
	{
		executorService = Executors.newSingleThreadExecutor();
	}

	@Override
	public void startImport(final File csvFile, final Boolean ediAdresOverschrijven)
	{
		executorService.submit(new Runnable()
		{
			@Override
			public void run()
			{
				SessionFactory sessionFactory = SpringBeanProvider.getInstance().getBean(SessionFactory.class);
				try (FileInputStream xlsStream = new FileInputStream(csvFile);)
				{
					Session session = sessionFactory.openSession();
					TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(session));
					importService.importHandmatigAdresboek(xlsStream, ediAdresOverschrijven);
				}
				catch (Exception e) 
				{
					LOG.error("Er is een onvoorziene crash geweest in de Zorgmail Import Thread, deze is nu gesloten. " + e.getMessage(), e);
					logService.logGebeurtenis(LogGebeurtenis.HUISARTS_IMPORT_FOUT, null,
						"Importeren van Huisartsen Adresboek is mislukt. Er is een crash opgetreden, alle gemaakte wijzigingen zijn terugggedraaid.", Bevolkingsonderzoek.COLON);
				}
				finally
				{
					SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.unbindResource(sessionFactory);
					SessionFactoryUtils.closeSession(sessionHolder.getSession());
				}
			}
		});
	}

	public ZorgmailImportService getImportService()
	{
		return importService;
	}

	public void setImportService(ZorgmailImportService importService)
	{
		this.importService = importService;
	}

}
