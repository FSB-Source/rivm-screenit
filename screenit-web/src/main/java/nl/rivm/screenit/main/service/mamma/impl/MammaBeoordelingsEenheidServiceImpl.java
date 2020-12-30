package nl.rivm.screenit.main.service.mamma.impl;

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

import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingsEenheidDao;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBeoordelingsEenheidServiceImpl implements MammaBeoordelingsEenheidService
{

	@Autowired
	private MammaBeoordelingsEenheidDao beoordelingsEenheidDao;

	@Autowired
	private InstellingDao instellingDao;

	@Override
	public long getAantalActieveGekoppeldeScreeningsEenheden(BeoordelingsEenheid beoordelingsEenheid)
	{
		return beoordelingsEenheidDao.getAantalActieveGekoppeldeScreeningsEenheden(beoordelingsEenheid);
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeOnderzoeken(BeoordelingsEenheid beoordelingsEenheid)
	{
		return beoordelingsEenheidDao.getAantalNietAfgerondeGekoppeldeOnderzoeken(beoordelingsEenheid);
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeBeoordelingen(BeoordelingsEenheid beoordelingsEenheid)
	{
		return beoordelingsEenheidDao.getAantalNietAfgerondeGekoppeldeBeoordelingen(beoordelingsEenheid);
	}

	@Override
	public String magWordenGeactiveerd(BeoordelingsEenheid beoordelingsEenheid)
	{
		if (beoordelingsEenheid.getParent() != null && !beoordelingsEenheid.getParent().getActief())
		{
			return "beoordelingsEenheid.activeren.inactiveCE";
		}
		return "";
	}

	@Override
	public String magWordenGeinactiveerd(BeoordelingsEenheid beoordelingsEenheid)
	{
		if (getAantalActieveGekoppeldeScreeningsEenheden(beoordelingsEenheid) > 0)
		{
			return "beoordelingsEenheid.inactiveren.actieveSE";
		}
		if (getAantalNietAfgerondeGekoppeldeOnderzoeken(beoordelingsEenheid) > 0)
		{
			return "beoordelingsEenheid.inactiveren.actieveOnderzoeken";
		}
		if (getAantalNietAfgerondeGekoppeldeBeoordelingen(beoordelingsEenheid) > 0)
		{
			return "beoordelingsEenheid.inactiveren.actieveBeoordelingen";
		}
		return "";
	}

	@Override
	public List<BeoordelingsEenheid> getBeoordelingsEenheden(Instelling instelling)
	{
		if (instelling == null)
		{
			return Collections.emptyList();
		}
		OrganisatieType organisatieType = instelling.getOrganisatieType();
		switch (organisatieType)
		{
		case RIVM:
		case KWALITEITSPLATFORM:
			return instellingDao.getActieveInstellingen(BeoordelingsEenheid.class);
		case BEOORDELINGSEENHEID:
			return Collections.singletonList((BeoordelingsEenheid) instelling);
		case SCREENINGSORGANISATIE:
			return beoordelingsEenheidDao.getActieveBeoordelingsEenhedenVoorScreeningsOrganisatie((ScreeningOrganisatie) instelling);
		default:
			return Collections.emptyList();
		}
	}

	@Override
	public List<BeoordelingsEenheid> getBeoordelingsEenheden(Instelling instelling, List<CentraleEenheid> centraleEenheden)
	{
		if (instelling == null)
		{
			return Collections.emptyList();
		}
		OrganisatieType organisatieType = instelling.getOrganisatieType();
		switch (organisatieType)
		{
		case RIVM:
		case KWALITEITSPLATFORM:
		case SCREENINGSORGANISATIE:
			return beoordelingsEenheidDao.getActieveBeoordelingsEenhedenVoorCentraleEenheden(centraleEenheden);
		case BEOORDELINGSEENHEID:
			return Collections.singletonList((BeoordelingsEenheid) instelling);
		default:
			return Collections.emptyList();
		}
	}
}
