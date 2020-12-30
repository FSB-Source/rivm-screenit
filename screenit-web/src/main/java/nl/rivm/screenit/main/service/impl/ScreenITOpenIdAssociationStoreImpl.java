package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Random;

import nl.rivm.screenit.dao.OpenIdAssocStoreDao;
import nl.rivm.screenit.main.service.ScreenITOpenIdAssociationStore;
import nl.rivm.screenit.model.OpenIdAssociation;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.openid4java.association.Association;
import org.openid4java.association.AssociationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Transactional(propagation = Propagation.SUPPORTS)
public class ScreenITOpenIdAssociationStoreImpl implements ScreenITOpenIdAssociationStore
{

	private static final Logger LOG = LoggerFactory.getLogger(ScreenITOpenIdAssociationStoreImpl.class);

	private static final int CLEANUP_INTERVAL = 60 * 1000;

	private static long laatsteCleanup = 0;

	private OpenIdAssocStoreDao assocStoreDao;

	private ICurrentDateSupplier dateSupplier;

	private HibernateService hibernateService;

	private SessionFactory sessionFactory;

	private static Random random;

	public ScreenITOpenIdAssociationStoreImpl()
	{
		assocStoreDao = SpringBeanProvider.getInstance().getBean(OpenIdAssocStoreDao.class);
		dateSupplier = SpringBeanProvider.getInstance().getBean(ICurrentDateSupplier.class);
		hibernateService = SpringBeanProvider.getInstance().getBean(HibernateService.class);
		sessionFactory = SpringBeanProvider.getInstance().getBean(SessionFactory.class);
		random = new Random(dateSupplier.getDate().getTime());
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public Association generate(String type, int expiryIn) throws AssociationException
	{
		Transaction transaction = null;
		SessionHolder holder = null;
		try
		{
			Object resource = TransactionSynchronizationManager.getResource(sessionFactory);
			if (resource instanceof SessionHolder)
			{
				holder = (SessionHolder) resource;
				if (holder.getTransaction() == null)
				{
					transaction = hibernateService.getHibernateSession().beginTransaction();
					holder.setTransaction(transaction);
				}
			}
			cleanupExpired();
			String handle = Long.toHexString(random.nextLong());
			Association associatie = Association.generate(type, handle, expiryIn);
			OpenIdAssociation colonAssoc = new OpenIdAssociation();
			colonAssoc.setHandle(associatie.getHandle());
			colonAssoc.setExpdate(associatie.getExpiry());
			colonAssoc.setType(associatie.getType());
			colonAssoc.setMackey(new String(Base64.encodeBase64(associatie.getMacKey().getEncoded())));
			assocStoreDao.saveAssoc(colonAssoc);

			LOG.debug("Associatie gegenereerd, handle: " + handle + " type: " + type + " verloopt in: " + expiryIn + " seconden.");
			return associatie;
		}
		catch (Exception e) 
		{
			LOG.error("Probleem opgetreden bij het aanmaken/opslaan van een assocatie", e);
			throw new AssociationException("Probleem opgetreden bij het aanmaken/opslaan van een assocatie.");
		}
		finally
		{
			if (transaction != null)
			{
				transaction.commit();
				holder.setTransaction(null);
			}
		}
	}

	@Transactional(propagation = Propagation.SUPPORTS)
	@Override
	public Association load(String handle)
	{
		try
		{
			OpenIdAssociation colonAssoc = assocStoreDao.loadByHandle(handle);
			if (colonAssoc == null)
			{
				throw new AssociationException("Associatie niet gevonden voor handle: " + handle);
			}
			Association associatie = null;
			if (StringUtils.equalsIgnoreCase(Association.TYPE_HMAC_SHA1, colonAssoc.getType()))
			{
				associatie = Association.createHmacSha1(handle, Base64.decodeBase64(colonAssoc.getMackey().getBytes()), colonAssoc.getExpdate());
			}
			else if (StringUtils.equalsIgnoreCase(Association.TYPE_HMAC_SHA256, colonAssoc.getType()))
			{
				associatie = Association.createHmacSha256(handle, Base64.decodeBase64(colonAssoc.getMackey().getBytes()), colonAssoc.getExpdate());
			}
			else
			{
				throw new AssociationException("Invalide associatie type gevonden: " + colonAssoc.getType());
			}
			LOG.debug("Associatie gevonden voor handle: " + handle);
			return associatie;
		}
		catch (AssociationException ase)
		{
			LOG.error("Er was een probleem bij het ophalen van de assocatie.", ase);
			return null;
		}

	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void remove(String handle)
	{
		Transaction transaction = null;
		SessionHolder holder = null;
		try
		{
			Object resource = TransactionSynchronizationManager.getResource(sessionFactory);
			if (resource instanceof SessionHolder)
			{
				holder = (SessionHolder) resource;
				if (holder.getTransaction() == null)
				{
					transaction = hibernateService.getHibernateSession().beginTransaction();
					holder.setTransaction(transaction);
				}
			}
			int aantal = assocStoreDao.removeByHandle(handle);
			if (aantal == 1)
			{
				LOG.debug("Associatie met handle: " + handle + " is verwijderd.");
			}
			else if (aantal != 1)
			{
				LOG.warn("Er was een probleem met verwijderen van assocatie met handle: " + handle + ". Er zijn " + aantal + " rijen aangepast");
			}
		}
		catch (Exception e) 
		{
			LOG.error("Er was een probleem tijdens verwijderen van een assocatie", e);
		}
		finally
		{
			if (transaction != null)
			{
				transaction.commit();
				holder.setTransaction(null);
			}
		}
	}

	private void cleanupExpired()
	{
		if (dateSupplier.getDate().getTime() - laatsteCleanup < CLEANUP_INTERVAL)
		{
			return;
		}
		try
		{
			int aantal = assocStoreDao.cleanupByDate(dateSupplier.getDate());
			LOG.debug("Er zijn " + aantal + " verlopen associaties verwijderd.");
			laatsteCleanup = dateSupplier.getDate().getTime();
		}
		catch (Exception e) 
		{
			LOG.error("Er is een probleem opgetreden tijdens het verwijderen van verlopen assocaties.", e);
		}
	}
}
