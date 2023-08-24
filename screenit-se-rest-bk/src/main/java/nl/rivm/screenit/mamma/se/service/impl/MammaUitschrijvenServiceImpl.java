package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.actions.UitschrijvenDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.MammaUitschrijvenService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MammaUitschrijvenServiceImpl implements MammaUitschrijvenService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Override
	public void uitschrijven(UitschrijvenDto action, InstellingGebruiker instellingGebruiker)
	{
		MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		afspraak.setStatus(MammaAfspraakStatus.GEPLAND);
		hibernateService.saveOrUpdate(afspraak);
	}
}
