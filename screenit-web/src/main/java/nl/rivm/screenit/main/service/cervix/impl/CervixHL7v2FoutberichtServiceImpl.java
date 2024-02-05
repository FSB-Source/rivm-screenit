package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.cervix.CervixHL7v2FoutberichtService;
import nl.rivm.screenit.model.cervix.berichten.CervixFoutHL7v2Bericht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixHL7v2FoutberichtServiceImpl implements CervixHL7v2FoutberichtService
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void berichtOpnieuwAanbieden(CervixFoutHL7v2Bericht foutbericht)
	{
		hibernateService.delete(foutbericht);
	}

	@Override
	public void verwijderBericht(CervixFoutHL7v2Bericht foutbericht)
	{
		hibernateService.delete(foutbericht);
		hibernateService.delete(foutbericht.getMessage());
	}
}
