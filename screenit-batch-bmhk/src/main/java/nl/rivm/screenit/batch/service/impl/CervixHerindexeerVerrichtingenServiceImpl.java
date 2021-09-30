package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.service.CervixHerindexeerVerrichtingenService;
import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.messagequeue.dto.CervixHerindexatieDto;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixHerindexeerVerrichtingenServiceImpl implements CervixHerindexeerVerrichtingenService
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixHerindexeerVerrichtingenServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private CervixVerrichtingDao verrichtingDao;

	@Autowired
	private LogService logService;

	private final static ObjectMapper objectMapper = new ObjectMapper();

	@Override
	public int verrichtingenHerindexeren(CervixHerindexatieDto herindexatieDto)
	{
		int aantalVerrichtingen = 0;
		CervixTarief oudeTarief = hibernateService.load(CervixTarief.class, herindexatieDto.getOudeTariefId());
		CervixTarief nieuweTarief = hibernateService.load(CervixTarief.class, herindexatieDto.getNieuweTariefId());
		if (herindexatieDto.isHuisartsTarief())
		{
			aantalVerrichtingen += bepaalBoekregelsVoorVerrichtingen(oudeTarief, nieuweTarief, CervixTariefType.HUISARTS_UITSTRIJKJE);
		}
		else
		{
			for (CervixTariefType labTariefType : CervixTariefType.getAlleLabTariefTypes())
			{
				if (!labTariefType.getBedragVanTarief(nieuweTarief).equals(labTariefType.getBedragVanTarief(oudeTarief)))
				{
					aantalVerrichtingen += bepaalBoekregelsVoorVerrichtingen(oudeTarief, nieuweTarief, labTariefType);
				}
			}
		}
		return aantalVerrichtingen;

	}

	private int bepaalBoekregelsVoorVerrichtingen(CervixTarief oudeTarief, CervixTarief nieuweTarief, CervixTariefType tariefType)
	{
		List<CervixVerrichting> verrichtingen = verrichtingDao.getVerrichtingenVoorTarief(oudeTarief.getId(), nieuweTarief, tariefType);
		verrichtingen.forEach(v -> bepaalBoekregelsVoorVerrichting(v, oudeTarief, nieuweTarief));
		return verrichtingen.size();
	}

	private void bepaalBoekregelsVoorVerrichting(CervixVerrichting verrichting, CervixTarief oudeTarief, CervixTarief nieuweTarief)
	{
		CervixBoekRegel laatsteBoekRegel = verrichting.getLaatsteBoekRegel();
		String logIndexeerd = "";

		if (laatsteBoekRegel.getSpecificatie() != null)
		{

			maakDebetEnCreditBoekregels(verrichting, oudeTarief, nieuweTarief);
			logIndexeerd = "Geherindexeerde ";
		}
		else
		{
			updateLaatsteBoekregel(verrichting, nieuweTarief, laatsteBoekRegel);
		}

		LOG.info(logIndexeerd + "Verrichting(" + verrichting.getId() + "): van " + CervixTariefUtil.getTariefString(oudeTarief) + " naar " + CervixTariefUtil
			.getTariefString(nieuweTarief));
		if (CervixTariefType.isHuisartsTarief(nieuweTarief))
		{
			huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(verrichting));
		}
	}

	private void updateLaatsteBoekregel(CervixVerrichting verrichting, CervixTarief nieuweTarief, CervixBoekRegel laatsteBoekRegel)
	{
		laatsteBoekRegel.setTarief(nieuweTarief);
		hibernateService.saveOrUpdate(laatsteBoekRegel);
		hibernateService.saveOrUpdate(verrichting);
	}

	private void maakDebetEnCreditBoekregels(CervixVerrichting verrichting, CervixTarief oudeTarief, CervixTarief nieuweTarief)
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
	}

	@Override
	public void logStart(CervixHerindexatieDto herindexatieDto)
	{
		CervixTarief oudeTarief = hibernateService.load(CervixTarief.class, herindexatieDto.getOudeTariefId());
		String melding = "Start herindexatie voor verrichtingen voor " + CervixTariefUtil.getTariefString(oudeTarief) + ".";
		LogGebeurtenis gebeurtenis = LogGebeurtenis.CERVIX_HUISARTS_TARIEF_INDEXEERD;
		if (!herindexatieDto.isHuisartsTarief())
		{
			CervixLabTarief labTarief = (CervixLabTarief) HibernateHelper.deproxy(oudeTarief);
			melding = "BMHK laboratorium: " + labTarief.getBmhkLaboratorium().getNaam() + "; " + melding;
			gebeurtenis = LogGebeurtenis.CERVIX_LAB_TARIEF_INDEXEERD;
		}
		try
		{
			LOG.info(melding + " " + objectMapper.writeValueAsString(herindexatieDto));
		}
		catch (JsonProcessingException e)
		{
			LOG.error("Fout bij maken string van herindexatieDto", e);
		}
		logService.logGebeurtenis(gebeurtenis, null, melding, Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public void logEinde(CervixHerindexatieDto herindexatieDto, int totaalAantalVerrichtingen)
	{
		CervixTarief oudeTarief = hibernateService.load(CervixTarief.class, herindexatieDto.getOudeTariefId());
		String melding = totaalAantalVerrichtingen + " verrichtingen voor " + CervixTariefUtil.getTariefString(oudeTarief) + " bijgewerkt.";
		LogGebeurtenis gebeurtenis = LogGebeurtenis.CERVIX_HUISARTS_TARIEF_INDEXEERD;
		if (!herindexatieDto.isHuisartsTarief())
		{
			CervixLabTarief labTarief = (CervixLabTarief) HibernateHelper.deproxy(oudeTarief);
			melding = "BMHK laboratorium: " + labTarief.getBmhkLaboratorium().getNaam() + "; " + melding;
			gebeurtenis = LogGebeurtenis.CERVIX_LAB_TARIEF_INDEXEERD;
		}
		LOG.info(melding);
		logService.logGebeurtenis(gebeurtenis, null, melding, Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public void logFout(CervixHerindexatieDto herindexatieDto, int totaalAantalVerrichtingen, Exception e)
	{
		LogGebeurtenis gebeurtenis = LogGebeurtenis.CERVIX_HUISARTS_TARIEF_INDEXEERD;
		String melding = "";
		if (herindexatieDto != null)
		{
			CervixTarief oudeTarief = (CervixTarief) HibernateHelper.deproxy(hibernateService.load(CervixTarief.class, herindexatieDto.getOudeTariefId()));

			melding += "Er zijn wel " + totaalAantalVerrichtingen + " verrichtingen voor " + CervixTariefUtil.getTariefString(oudeTarief) + " bijgewerkt.";

			if (!herindexatieDto.isHuisartsTarief())
			{
				CervixLabTarief labTarief = (CervixLabTarief) oudeTarief;
				melding = "BMHK laboratorium: " + labTarief.getBmhkLaboratorium().getNaam() + "; " + melding;
				gebeurtenis = LogGebeurtenis.CERVIX_LAB_TARIEF_INDEXEERD;
			}
			LOG.error("Fout bij verwerking. " + melding, e);
		}
		else
		{
			LOG.error("Fout bij verwerking door: ", e);
		}
		LogEvent logEvent = new LogEvent();
		logEvent.setLevel(Level.ERROR);
		logEvent.setMelding("Er is een onbekende fout opgetreden tijdens het verwerken van herindexatie, neem contact op met Topicus. " + melding);
		logService.logGebeurtenis(gebeurtenis, logEvent, Bevolkingsonderzoek.CERVIX);
	}

}
