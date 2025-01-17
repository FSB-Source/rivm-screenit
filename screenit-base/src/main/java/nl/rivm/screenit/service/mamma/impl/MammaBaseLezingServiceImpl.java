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

import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAfbeeldingZijdeDoorsnede;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.mamma.MammaBaseLezingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseLezingServiceImpl implements MammaBaseLezingService
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public boolean isZijdeGeamputeerd(MammaAfbeeldingZijdeDoorsnede doorsnede, MammaAmputatie amputatie)
	{
		return amputatie != null
			&& ((MammaAfbeeldingZijdeDoorsnede.LINKS_HORIZONTALE_DOORSNEDE.equals(doorsnede) || MammaAfbeeldingZijdeDoorsnede.LINKS_VERTICALE_DOORSNEDE.equals(doorsnede))
			&& MammaAmputatie.LINKERBORST.equals(amputatie))
			|| ((MammaAfbeeldingZijdeDoorsnede.RECHTS_HORIZONTALE_DOORSNEDE.equals(doorsnede) || MammaAfbeeldingZijdeDoorsnede.RECHTS_VERTICALE_DOORSNEDE.equals(doorsnede))
			&& MammaAmputatie.RECHTERBORST.equals(amputatie));
	}

	@Override
	public boolean isZijdeGeamputeerd(MammaZijde zijde, MammaAmputatie amputatie)
	{
		return amputatie != null && ((MammaZijde.LINKER_BORST.equals(zijde) && MammaAmputatie.LINKERBORST.equals(amputatie))
			|| (MammaZijde.RECHTER_BORST.equals(zijde) && MammaAmputatie.RECHTERBORST.equals(amputatie)));
	}

	@Override
	public MammaZijde bepaalZijdeMetPrioriteit(MammaLezing lezing)
	{
		var biradsLinks = lezing.getBiradsLinks();
		var biradsRechts = lezing.getBiradsRechts();
		if (biradsLinks != null && biradsRechts != null)
		{
			return bepaalBelangrijksteZijde(biradsLinks, biradsRechts);
		}
		return MammaZijde.RECHTER_BORST;
	}

	private MammaZijde bepaalBelangrijksteZijde(MammaBIRADSWaarde links, MammaBIRADSWaarde rechts)
	{
		if (links.getPrio() > rechts.getPrio())
		{
			return MammaZijde.LINKER_BORST;
		}
		return MammaZijde.RECHTER_BORST;
	}

}
