package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.service.BaseScreeningRondeService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class BaseScreeningRondeServiceImpl implements BaseScreeningRondeService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void heropenScreeningRonde(ScreeningRonde ronde)
	{
		if (ronde != null && ScreeningRondeStatus.AFGEROND.equals(ronde.getStatus()))
		{
			ronde.setStatus(ScreeningRondeStatus.LOPEND);
			DateTime nu = currentDateSupplier.getDateTime();
			ronde.setStatusDatum(nu.plusMillis(200).toDate());
			ronde.setAangemeld(true);
			ronde.setAfgerondReden(null);

			hibernateService.saveOrUpdate(ronde);
		}
	}

	@Override
	public void screeningRondeAfronden(ScreeningRonde ronde)
	{
		ronde.setStatus(ScreeningRondeStatus.AFGEROND);
		ronde.setStatusDatum(currentDateSupplier.getDate());

		hibernateService.saveOrUpdate(ronde);
	}

	public void setCurrentDateSupplier(ICurrentDateSupplier currentDateSupplier)
	{
		this.currentDateSupplier = currentDateSupplier;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}
}
