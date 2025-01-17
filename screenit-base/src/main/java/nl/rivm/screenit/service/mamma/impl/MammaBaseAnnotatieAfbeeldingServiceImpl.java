package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaAnnotatieIcoon;
import nl.rivm.screenit.service.mamma.MammaBaseAnnotatieAfbeeldingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseAnnotatieAfbeeldingServiceImpl implements MammaBaseAnnotatieAfbeeldingService
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void updateIconenInAfbeelding(List<MammaAnnotatieIcoon> iconen, MammaAnnotatieAfbeelding afbeelding)
	{
		hibernateService.deleteAll(afbeelding.getIconen());
		afbeelding.setIconen(iconen);
		iconen.forEach(icoon -> icoon.setAfbeelding(afbeelding));

		hibernateService.saveOrUpdate(afbeelding);
		iconen.forEach(icoon -> hibernateService.save(icoon));
	}
}
