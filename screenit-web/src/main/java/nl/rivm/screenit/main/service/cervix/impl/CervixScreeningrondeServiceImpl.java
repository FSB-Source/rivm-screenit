
package nl.rivm.screenit.main.service.cervix.impl;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.cervix.CervixScreeningrondeDao;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.cervix.CervixScreeningrondeService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixScreeningrondeServiceImpl implements CervixScreeningrondeService
{

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private BriefService briefService;

	@Autowired
	private CervixScreeningrondeDao screeningrondeDao;

	@Override
	public void maakHerdruk(CervixBrief cervixBrief, Account account)
	{
		briefService.opnieuwAanmaken(cervixBrief, account);
	}

	@Override
	public boolean isBriefMetZelfdeBriefTypeAangemaakt(CervixBrief brief)
	{
		boolean heeftAangemaakteBrief = false;
		for (CervixBrief andereBrief : brief.getScreeningRonde().getBrieven())
		{
			if (andereBrief.getBriefType().equals(brief.getBriefType()) && !andereBrief.isGegenereerd())
			{

				heeftAangemaakteBrief = true;
				break;
			}
		}
		return heeftAangemaakteBrief;
	}

	@Override
	public boolean heeftMaxAantalZASsenBereikt(CervixScreeningRonde laatsteScreeningRonde, boolean aangevraagdDoorClient)
	{
		Integer maxAantalZASaanvragen = getMaxAantalZASAanvragen(aangevraagdDoorClient);

		return screeningrondeDao.getAantalZASsenAangevraagd(laatsteScreeningRonde, aangevraagdDoorClient) >= maxAantalZASaanvragen;
	}

	@Override
	public Integer getMaxAantalZASAanvragen(boolean aangevraagdDoorClient)
	{
		Integer maxAantalZASaanvragen = null;
		if (aangevraagdDoorClient)
		{
			maxAantalZASaanvragen = preferenceService.getInteger(PreferenceKey.CERVIX_MAX_ZAS_AANVRAGEN_CLIENT.name());
		}
		else
		{
			maxAantalZASaanvragen = preferenceService.getInteger(PreferenceKey.CERVIX_MAX_ZAS_AANVRAGEN_INFOLIJN.name());
		}
		return maxAantalZASaanvragen;
	}
}
