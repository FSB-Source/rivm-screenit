
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.BaseHoudbaarheidDao;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BaseHoudbaarheidServiceImpl implements BaseHoudbaarheidService
{
	@Autowired
	private BaseHoudbaarheidDao houdbaarheidDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Override
	public <H extends AbstractHoudbaarheid> boolean isHoudbaar(Class<H> clazz, String barcode)
	{
		H houdbaarheid = houdbaarheidDao.getHoudbaarheidVoor(clazz, barcode);
		Date nu = currentDateSupplier.getDate();
		return houdbaarheid != null && !houdbaarheid.getVervalDatum().before(nu);
	}

	@Override
	public DateTime getMinstensHoudbaarTotMet(DateTime nu, PreferenceKey minimaleHoudbaarheidMonstersVoorControleKey)
	{
		Integer periodeMinimaleHoudbaarheidIfobtMonstersVoorControle = simplePreferenceService.getInteger(minimaleHoudbaarheidMonstersVoorControleKey.name());
		if (periodeMinimaleHoudbaarheidIfobtMonstersVoorControle == null)
		{
			periodeMinimaleHoudbaarheidIfobtMonstersVoorControle = Integer.valueOf(61);
		}
		else
		{
			periodeMinimaleHoudbaarheidIfobtMonstersVoorControle++;
		}
		DateTime minstensHoudbaarTotMet = nu.plusDays(periodeMinimaleHoudbaarheidIfobtMonstersVoorControle).withTimeAtStartOfDay().toDateTime();
		return minstensHoudbaarTotMet;
	}
}
