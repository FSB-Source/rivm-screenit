package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixAsyncIndexatieService;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.annotation.HibernateSession;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixAsyncIndexatieServiceImpl implements CervixAsyncIndexatieService
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixAsyncIndexatieServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixVerrichtingDao verrichtingDao;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private LogService logService;

	@HibernateSession
	@Async
	@Override
	public void indexeren(long oudeTariefId, long nieuweTariefId, boolean isHuisartsTarief)
	{
		CervixTarief oudeTarief;
		CervixTarief nieuweTarief;

		LOG.info("Verrichtingen voor tarief " + oudeTariefId + " worden aangepast.");
		if (isHuisartsTarief)
		{
			oudeTarief = hibernateService.load(CervixHuisartsTarief.class, oudeTariefId);
			nieuweTarief = hibernateService.load(CervixHuisartsTarief.class, nieuweTariefId);
		}
		else
		{
			oudeTarief = hibernateService.load(CervixLabTarief.class, oudeTariefId);
			nieuweTarief = hibernateService.load(CervixLabTarief.class, nieuweTariefId);
		}

		int aantalVerrichtingen = 0;
		if (isHuisartsTarief)
		{
			List<CervixVerrichting> verrichtingen = verrichtingDao.getVerrichtingenVoorTarief(oudeTariefId, nieuweTarief, CervixTariefType.HUISARTS_UITSTRIJKJE);
			verrichtingen.forEach(v -> indexatieRegelsToevoegenVoorVerrichting(v, oudeTarief, nieuweTarief));
			aantalVerrichtingen += verrichtingen.size();
		}
		else
		{
			for (CervixTariefType labTariefType : CervixTariefType.getAlleLabTariefTypes())
			{
				if (!labTariefType.getBedragVanTarief(nieuweTarief).equals(labTariefType.getBedragVanTarief(oudeTarief)))
				{
					List<CervixVerrichting> verrichtingen = verrichtingDao.getVerrichtingenVoorTarief(oudeTariefId, nieuweTarief, labTariefType);
					verrichtingen.forEach(v -> indexatieRegelsToevoegenVoorVerrichting(v, oudeTarief, nieuweTarief));
					aantalVerrichtingen += verrichtingen.size();
				}
			}
		}
		LOG.info(aantalVerrichtingen + " verrichtingen voor tarief '" + CervixTariefUtil.getTariefString(oudeTarief) + "' zijn aangepast.");
		String melding = aantalVerrichtingen + " verrichtingen voor \"" + CervixTariefUtil.getTariefString(oudeTarief) + "\" bijgewerkt.";
		if (isHuisartsTarief)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTS_TARIEF_INDEXEERD, null, melding, Bevolkingsonderzoek.CERVIX);
		}
		else
		{
			CervixLabTarief labTarief = (CervixLabTarief) HibernateHelper.deproxy(oudeTarief);
			melding = "BMHK laboratorium: " + labTarief.getBmhkLaboratorium().getNaam() + "; " + melding;
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LAB_TARIEF_INDEXEERD, null, melding, Bevolkingsonderzoek.CERVIX);
		}
	}

	private void indexatieRegelsToevoegenVoorVerrichting(CervixVerrichting verrichting, CervixTarief oudeTarief, CervixTarief nieuweTarief)
	{
		CervixBoekRegel laatsteBoekRegel = verrichting.getLaatsteBoekRegel();
		String logIndexeerd = "";

		if (laatsteBoekRegel.getSpecificatie() != null)
		{
			CervixBoekRegel debetRegel = new CervixBoekRegel();
			debetRegel.setVerrichting(verrichting);
			debetRegel.setDebet(true);
			debetRegel.setTarief(oudeTarief);
			hibernateService.saveOrUpdate(debetRegel);
			verrichting.getBoekRegels().add(debetRegel);

			CervixBoekRegel creditRegel = new CervixBoekRegel();
			creditRegel.setVerrichting(verrichting);
			creditRegel.setTarief(nieuweTarief);
			creditRegel.setDebet(false);
			hibernateService.saveOrUpdate(creditRegel);
			verrichting.setLaatsteBoekRegel(creditRegel);

			verrichting.getBoekRegels().add(creditRegel);
			hibernateService.saveOrUpdate(verrichting);
			logIndexeerd = "Geindexeerde ";
		}
		else
		{
			laatsteBoekRegel.setTarief(nieuweTarief);
			hibernateService.saveOrUpdate(laatsteBoekRegel);
			hibernateService.saveOrUpdate(verrichting);
		}

		LOG.info(logIndexeerd + "Verrichting(" + verrichting.getId() + "): van " + CervixTariefUtil.getTariefString(oudeTarief) + " naar "
			+ CervixTariefUtil.getTariefString(nieuweTarief) + ")");
		if (CervixTariefType.isHuisartsTarief(nieuweTarief))
		{
			huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(verrichting));
		}
	}
}
