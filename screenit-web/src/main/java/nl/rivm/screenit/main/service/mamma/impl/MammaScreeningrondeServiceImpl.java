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

import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.mamma.MammaScreeningrondeService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaScreeningrondeServiceImpl implements MammaScreeningrondeService
{

	@Autowired
	private BriefService briefService;

	@Override
	public void maakHerdruk(MammaBrief mammaBrief, Account account)
	{
		briefService.opnieuwAanmaken(mammaBrief, account);
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public boolean isBriefMetZelfdeBriefTypeAangemaakt(MammaBrief brief)
	{
		boolean heeftAangemaakteBrief = false;
		for (MammaBrief andereBrief : brief.getScreeningRonde().getBrieven())
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
	public boolean heeftGeprinteOfTegengehoudenUitslagBrief(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde.getBrieven().stream().anyMatch(
			brief -> BriefType.isMammaUitslagBrief(brief.getBriefType()) &&
				(brief.getMergedBrieven() != null && brief.getMergedBrieven().getGeprint()
					|| brief.getMergedBrieven() == null && brief.isGegenereerd()
					|| brief.isTegenhouden()));
	}
}
