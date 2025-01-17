package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.service.HeraanmeldenMergeVeldService;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class HeraanmeldenMergeVeldServiceImpl implements HeraanmeldenMergeVeldService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public String getValueVanHeraanmeldenTekstKey(ClientBrief brief)
	{
		ClientBrief herdruk = BriefUtil.getHerdruk(brief);
		if (herdruk != null)
		{
			return getValueVanHeraanmeldenTekstKey(herdruk);
		}

		PreferenceKey heraanmeldenTekstKey = getBriefPreferenceKey(brief);
		if (heraanmeldenTekstKey != null)
		{
			return preferenceService.getString(heraanmeldenTekstKey.toString());
		}
		return null;
	}

	private PreferenceKey getBriefPreferenceKey(ClientBrief clientBrief)
	{
		ClientBrief brief = (ClientBrief) HibernateHelper.deproxy(clientBrief);
		if (brief instanceof ColonBrief)
		{
			IFOBTTest ifobtTest = ((ColonBrief) brief).getIfobtTest();
			return ifobtTest != null ? ifobtTest.getHeraanmeldenTekstKey() : null;
		}
		else if (brief instanceof CervixBrief)
		{
			return ((CervixBrief) brief).getHeraanmeldenTekstKey();
		}
		else
		{
			return null;
		}
	}

}
