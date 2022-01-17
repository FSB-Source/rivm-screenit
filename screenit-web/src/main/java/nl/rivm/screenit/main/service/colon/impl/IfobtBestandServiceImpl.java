
package nl.rivm.screenit.main.service.colon.impl;

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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.dao.IfobtBestandDao;
import nl.rivm.screenit.main.model.colon.IFobtBatchFilter;
import nl.rivm.screenit.main.service.colon.IfobtBestandService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class IfobtBestandServiceImpl implements IfobtBestandService
{

	@Autowired
	private IfobtBestandDao ifobtBestandDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Override
	public Iterator<? extends IFOBTBestand> getBestanden(IFobtBatchFilter filter, long first, long count, String sortProperty, boolean ascending)
	{
		return ifobtBestandDao.getBestanden(filter, first, count, sortProperty, ascending);
	}

	@Override
	public long countBestanden(IFobtBatchFilter filter)
	{
		return ifobtBestandDao.countBestanden(filter);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderBestanden(List<IFOBTBestand> ifobtBestanden, Account ingelogdeAccount)
	{
		IFobtLaboratorium lab = null;
		for (IFOBTBestand bestand : ifobtBestanden)
		{
			bestand.setStatus(IFOBTBestandStatus.VERWIJDERD);
			lab = bestand.getLaboratorium();
			hibernateService.saveOrUpdate(bestand);
		}
		if (lab != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_BESTANDEN_VERWIJDERD, ingelogdeAccount, "Labid qbase " + lab.getQbasenummer(), Bevolkingsonderzoek.COLON);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void autoriseerBestanden(List<IFOBTBestand> ifobtBestanden, Account ingelogdeAccount)
	{
		IFobtLaboratorium lab = null;
		for (IFOBTBestand bestand : ifobtBestanden)
		{
			lab = bestand.getLaboratorium();
			bestand.setStatus(IFOBTBestandStatus.GEAUTORISEERD);
			hibernateService.saveOrUpdate(bestand);
		}
		if (lab != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_BESTANDEN_GEAUTORISEERD, ingelogdeAccount, "Labid qbase " + lab.getQbasenummer(), Bevolkingsonderzoek.COLON);
		}
	}

}
