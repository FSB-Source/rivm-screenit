package nl.rivm.screenit.main.service.mamma.impl;

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

import nl.rivm.screenit.main.service.mamma.MammaMammograafService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaMammograafServiceImpl implements MammaMammograafService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Override
	public boolean saveOrUpdateMammograaf(MammaMammograaf mammograaf, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "";
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(mammograaf, hibernateService.getHibernateSession());

		boolean isNieuw = mammograaf.getId() == null;
		if (isNieuw)
		{
			melding += mammograafAanduiding(mammograaf) + " aangemaakt met werkstation IP-adres " + mammograaf.getWerkstationIpAdres();
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += mammograafAanduiding(mammograaf) + " gewijzigd (" + diffToLatestVersion + ").";
		}

		if (StringUtils.isNotEmpty(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_MAMMOGRAAF, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdate(mammograaf);
			return true;
		}
		return false;
	}

	private String mammograafAanduiding(MammaMammograaf mammograaf)
	{
		return "Mammograaf " + mammograaf.getAeTitle();
	}
}
