
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.main.dao.OngeldigeBerichtenDao;
import nl.rivm.screenit.main.service.BerichtenZoekFilter;
import nl.rivm.screenit.main.service.OngeldigeBerichtenService;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
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
@Transactional(propagation = Propagation.SUPPORTS)
public class OngeldigeBerichtenServiceImpl implements OngeldigeBerichtenService
{

	private static final Logger LOG = LoggerFactory.getLogger(OngeldigeBerichtenServiceImpl.class);

	@Autowired
	private OngeldigeBerichtenDao ongeldigeBerichtenDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BerichtToBatchService cdaBerichtToBatchService;

	private ExecutorService executorService;

	public OngeldigeBerichtenServiceImpl()
	{
		executorService = Executors.newSingleThreadExecutor();
	}

	@Override
	public List<MeldingOngeldigCdaBericht> searchOngeldigeBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending)
	{

		return ongeldigeBerichtenDao.searchOngeldigeBerichten(filter, first, count, property, ascending);
	}

	@Override
	public long countOngeldigeBerichten(BerichtZoekFilter filter)
	{
		return ongeldigeBerichtenDao.countOngeldigeBerichten(filter);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void berichtOpnieuwAanbieden(MeldingOngeldigCdaBericht melding)
	{
		BerichtType berichtType = melding.getOntvangenCdaBericht().getBerichtType();
		Long id = melding.getOntvangenCdaBericht().getId();
		switch (berichtType)
		{
		case MDL_VERSLAG:
		case PA_LAB_VERSLAG:
			cdaBerichtToBatchService.queueColonCDABericht(id);
			break;
		case CERVIX_CYTOLOGIE_VERSLAG:
			cdaBerichtToBatchService.queueCervixCDABericht(id);
			break;
		case MAMMA_PA_FOLLOW_UP_VERSLAG:
			cdaBerichtToBatchService.queueMammaCDABericht(id);
			break;
		}
		LOG.info("CDA bericht " + melding.getOntvangenCdaBericht().getId() + " in MeldingOngeldigCdaBericht " + melding.getId() + " opnieuw aangeboden aan batch");
		verwijderenOngeldigBericht(melding);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderenOngeldigBericht(MeldingOngeldigCdaBericht meldingOngeldigCdaBericht)
	{
		meldingOngeldigCdaBericht.setActief(false);
		hibernateService.saveOrUpdate(meldingOngeldigCdaBericht);
		LOG.info("MeldingOngeldigCdaBericht " + meldingOngeldigCdaBericht.getId() + " gedeactiveerd");
	}

	@Override
	public void herverwerkAlleBerichten(BerichtenZoekFilter berichtFilter)
	{
		List<MeldingOngeldigCdaBericht> ongeldigeBerichten = ongeldigeBerichtenDao.searchOngeldigeBerichten(berichtFilter, -1, -1, "datum", false);

		final List<Long> ongeldigeBerichtenIds = new ArrayList<>();
		for (MeldingOngeldigCdaBericht ongeldigCdaBericht : ongeldigeBerichten)
		{
			if (Boolean.TRUE.equals(ongeldigCdaBericht.getActief()) && Boolean.TRUE.equals(ongeldigCdaBericht.getHerstelbaar()))
			{
				ongeldigeBerichtenIds.add(ongeldigCdaBericht.getId());
			}
		}

		LOG.info("Er worden nu " + ongeldigeBerichtenIds.size() + " berichten opnieuw aangeboden in een Thread.");
		executorService.submit(new Runnable()
		{
			@Override
			public void run()
			{
				SessionFactory sessionFactory = SpringBeanProvider.getInstance().getBean(SessionFactory.class);
				try
				{
					Session session = sessionFactory.openSession();
					TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(session));

					for (Long ongeldigBerichtId : ongeldigeBerichtenIds)
					{
						MeldingOngeldigCdaBericht ongeldigCdaBericht = SpringBeanProvider.getInstance().getBean(HibernateService.class).load(MeldingOngeldigCdaBericht.class,
							ongeldigBerichtId);
						SpringBeanProvider.getInstance().getBean(OngeldigeBerichtenService.class).berichtOpnieuwAanbieden(ongeldigCdaBericht);
						Thread.sleep(1000);
					}
				}
				catch (Exception e) 
				{
					LOG.error("Er is een onvoorziene crash geweest in de herverwerkAlleBerichten Thread, deze is nu gesloten. " + e.getMessage(), e);
				}
				finally
				{
					SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.unbindResource(sessionFactory);
					SessionFactoryUtils.closeSession(sessionHolder.getSession());
				}
			}
		});

	}
}
