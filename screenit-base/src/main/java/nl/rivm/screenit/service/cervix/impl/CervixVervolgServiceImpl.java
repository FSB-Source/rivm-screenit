package nl.rivm.screenit.service.cervix.impl;

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

import java.time.LocalDate;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixMonsterService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CervixVervolgServiceImpl implements CervixVervolgService
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private CervixBepaalVervolgDao bepaalVervolgDao;

	@Autowired
	private BaseHoudbaarheidService houdbaarheidService;

	@Autowired
	private CervixMonsterService monsterService;

	@Override
	public CervixVervolg bepaalVervolg(CervixMonster monster, LocalDate startdatumGenotypering)
	{
		return bepaalVervolg(monster, startdatumGenotypering, false);
	}

	@Override
	public CervixVervolg bepaalVervolg(CervixMonster monster, LocalDate startdatumGenotypering, boolean digitaalLabformulier)
	{
		boolean isZasHoudbaar = false;
		if (monster instanceof CervixZas)
		{
			isZasHoudbaar = houdbaarheidService.isHoudbaar(CervixZasHoudbaarheid.class, monster.getMonsterId());
		}

		return new CervixBepaalVervolgLabproces(
			new CervixBepaalVervolgContext(monster, isZasHoudbaar, dateSupplier.getLocalDateTime(), startdatumGenotypering, bepaalVervolgDao, monsterService,
				preferenceService.getInteger(PreferenceKey.CERVIX_INTERVAL_CONTROLE_UITSTRIJKJE.name()), digitaalLabformulier)).bepaalVervolg();
	}
}
