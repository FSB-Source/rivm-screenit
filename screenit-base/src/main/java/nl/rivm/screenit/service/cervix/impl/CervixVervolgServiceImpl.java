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
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixMonsterService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
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

	@Autowired
	private HibernateService hibernateService;

	@Override
	public CervixVervolg bepaalVervolg(CervixMonster monster, LocalDate startdatumGenotypering)
	{
		return bepaalVervolg(monster, startdatumGenotypering, false);
	}

	@Override
	public CervixVervolg bepaalVervolg(CervixMonster monster, LocalDate startdatumGenotypering, boolean digitaalLabformulier)
	{
		boolean isZasHoudbaar = false;
		if (CervixMonsterUtil.isZAS(monster))
		{
			isZasHoudbaar = houdbaarheidService.isHoudbaar(CervixZasHoudbaarheid.class, monster.getMonsterId());
		}

		return new CervixBepaalVervolgLabproces(
			new CervixBepaalVervolgContext(monster, isZasHoudbaar, dateSupplier.getLocalDateTime(), startdatumGenotypering, bepaalVervolgDao, monsterService,
				preferenceService.getInteger(PreferenceKey.CERVIX_INTERVAL_CONTROLE_UITSTRIJKJE.name()), digitaalLabformulier)).bepaalVervolg();
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void digitaalLabformulierKlaarVoorCytologie(CervixMonster monster)
	{
		if (!CervixMonsterUtil.isUitstrijkje(monster))
		{
			return;
		}

		CervixLabformulier labformulier = CervixMonsterUtil.getUitstrijkje(monster).getLabformulier();

		if (labformulier != null && labformulier.getDigitaal())
		{
			CervixVervolgTekst vervolgTekst = bepaalVervolg(monster, null, false).getVervolgTekst();
			digitaalLabformulierKlaarVoorCytologie(vervolgTekst, labformulier);
		}
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void digitaalLabformulierKlaarVoorCytologie(CervixMonster monster, CervixVervolgTekst vervolgTekst)
	{
		if (!CervixMonsterUtil.isUitstrijkje(monster))
		{
			return;
		}

		CervixLabformulier labformulier = CervixMonsterUtil.getUitstrijkje(monster).getLabformulier();

		if (labformulier != null && labformulier.getDigitaal())
		{
			digitaalLabformulierKlaarVoorCytologie(vervolgTekst, labformulier);
		}

	}

	private void digitaalLabformulierKlaarVoorCytologie(CervixVervolgTekst vervolgTekst, CervixLabformulier labformulier)
	{
		if (labformulier.getStatus() != CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE &&
			(vervolgTekst == CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_NAAR_CYTOLOGIE
				|| vervolgTekst == CervixVervolgTekst.UITSTRIJKJE_REEDS_HPV_UITSLAG_NAAR_CYTOLOGIE
				|| vervolgTekst == CervixVervolgTekst.UITSTRIJKJE_VERVOLGONDERZOEK_NAAR_CYTOLOGIE))
		{
			labformulier.setStatus(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE);
			hibernateService.saveOrUpdate(labformulier);
		}
	}
}
