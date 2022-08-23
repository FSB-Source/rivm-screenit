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

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.cervix.CervixHpvAnalyseresultaten;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.service.cervix.CervixMonsterService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixMonsterServiceImpl implements CervixMonsterService
{

	@Autowired
	private CervixMonsterDao monsterDao;

	@Override
	public CervixUitstrijkje getUitstrijkjeByClientBsnAndMonsterId(String bsn, String monsterId)
	{
		return monsterDao.getUitstrijkjeByClientBsnAndMonsterId(bsn, monsterId);
	}

	@Override
	public CervixUitstrijkje getUitstrijkjeByClientBsnAndControleLetters(String bsn, String controleLetters)
	{
		return monsterDao.getUitstrijkjeByClientBsnAndControleLetters(bsn, controleLetters);
	}

	@Override
	public CervixUitstrijkje getUitstrijkje(String monsterId)
	{
		return monsterDao.getUitstrijkje(monsterId);
	}

	@Override
	public CervixZas getZas(String monsterId)
	{
		return monsterDao.getZas(monsterId);
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public Long getNextMonsterId()
	{
		return monsterDao.getNextMonsterId();
	}

	@Override
	public boolean monsterHeeftHpvBeoordelingMetGenotypeOther(CervixMonster monsterHpvUitslag)
	{
		CervixHpvAnalyseresultaten analyseresultaten = monsterHpvUitslag.getLaatsteHpvBeoordeling().getAnalyseresultaten();

		return analyseresultaten != null && !CervixHpvResultValue.POS_HPV16.equals(analyseresultaten.getHpv16()) && !CervixHpvResultValue.POS_HPV18.equals(
			analyseresultaten.getHpv18()) && CervixHpvResultValue.POS_OTHER_HR_HPV.equals(analyseresultaten.getHpvohr());
	}
}
