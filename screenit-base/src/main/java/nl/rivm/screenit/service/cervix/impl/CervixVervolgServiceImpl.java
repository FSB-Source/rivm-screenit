package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHL7v24HpvOrderTriggerDto;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
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
	private CervixBaseMonsterService monsterService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Autowired
	private MessageService messageService;

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

	@Override
	public void sendHpvOrder(CervixMonster monster, CervixVervolgTekst vervolgTekst, BMHKLaboratorium bmhkLaboratorium)
	{
		boolean triggerHpvOrder = false;
		var cancelOrder = new AtomicBoolean(true);
		if (organisatieParameterService.getOrganisatieParameter(getBmhkLaboratorium(monster, bmhkLaboratorium), OrganisatieParameterKey.CERVIX_HPV_ORDER_NIEUW, Boolean.FALSE))
		{
			if (monster.getUitnodiging().getMonsterType() == CervixMonsterType.UITSTRIJKJE)
			{
				triggerHpvOrder = triggerHpvOrderVoorUitstrijkje(monster, vervolgTekst, cancelOrder);
			}
			else
			{
				triggerHpvOrder = triggerHpvOrderVoorZAS(monster, vervolgTekst, cancelOrder);
			}
		}
		if (triggerHpvOrder)
		{
			maakEnQueueHpvOrderMessageTrigger(monster, bmhkLaboratorium, cancelOrder.get());
		}

	}

	private boolean triggerHpvOrderVoorZAS(CervixMonster monster, CervixVervolgTekst vervolgTekst, AtomicBoolean cancelOrder)
	{
		boolean triggerHpvOrder = false;
		CervixZas zas = CervixMonsterUtil.getZAS(monster);
		var vorigeZasVersie = EntityAuditUtil.getPreviousVersionOfEntity(zas, hibernateService.getHibernateSession());
		CervixZasStatus vorigeZasStatus = null;
		if (vorigeZasVersie != null)
		{
			vorigeZasStatus = vorigeZasVersie.getZasStatus();
		}
		CervixZasStatus nieuweZasStatus = zas.getZasStatus();
		LOG.debug("Monster status change {}, {}=>{}", zas.getMonsterId(), vorigeZasStatus, nieuweZasStatus);
		if ((vorigeZasStatus == null || nieuweZasStatus != vorigeZasStatus) && nieuweZasStatus == CervixZasStatus.ONTVANGEN && vervolgTekst.isVoorHpvOrder())
		{
			cancelOrder.set(false);
			triggerHpvOrder = true;
		}
		else if (vorigeZasStatus != null && vorigeZasStatus == CervixZasStatus.ONTVANGEN &&
			(nieuweZasStatus == CervixZasStatus.VERSTUURD || nieuweZasStatus == CervixZasStatus.NIET_ANALYSEERBAAR))
		{
			triggerHpvOrder = true;
		}
		return triggerHpvOrder;
	}

	private boolean triggerHpvOrderVoorUitstrijkje(CervixMonster monster, CervixVervolgTekst vervolgTekst, AtomicBoolean cancelOrder)
	{
		boolean triggerHpvOrder = false;
		CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
		var vorigeUitstrijkjeVersie = EntityAuditUtil.getPreviousVersionOfEntity(uitstrijkje, hibernateService.getHibernateSession());
		CervixUitstrijkjeStatus vorigeUitstrijkjeStatus = null;
		if (vorigeUitstrijkjeVersie != null)
		{
			vorigeUitstrijkjeStatus = vorigeUitstrijkjeVersie.getUitstrijkjeStatus();
		}
		CervixUitstrijkjeStatus nieuweUitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
		LOG.debug("Monster status change {}, {}=>{}", uitstrijkje.getMonsterId(), vorigeUitstrijkjeStatus, nieuweUitstrijkjeStatus);
		if ((vorigeUitstrijkjeStatus == null || nieuweUitstrijkjeStatus != vorigeUitstrijkjeStatus) && nieuweUitstrijkjeStatus == CervixUitstrijkjeStatus.ONTVANGEN
			&& vervolgTekst.isVoorHpvOrder())
		{
			cancelOrder.set(false);
			triggerHpvOrder = true;
		}
		else if (vorigeUitstrijkjeStatus != null && vorigeUitstrijkjeStatus == CervixUitstrijkjeStatus.ONTVANGEN &&
			(nieuweUitstrijkjeStatus == CervixUitstrijkjeStatus.NIET_ONTVANGEN || nieuweUitstrijkjeStatus == CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR))
		{
			triggerHpvOrder = true;
		}
		return triggerHpvOrder;
	}

	private void maakEnQueueHpvOrderMessageTrigger(CervixMonster monster, BMHKLaboratorium bmhkLaboratorium, boolean cancelOrder)
	{
		CervixHL7v24HpvOrderTriggerDto triggerDto = new CervixHL7v24HpvOrderTriggerDto();
		triggerDto.setClazz(((CervixMonster) HibernateHelper.deproxy(monster)).getClass());
		triggerDto.setMonsterId(monster.getId());
		triggerDto.setCancelOrder(cancelOrder);
		bmhkLaboratorium = getBmhkLaboratorium(monster, bmhkLaboratorium);
		messageService.queueMessage(MessageType.HPV_ORDER, triggerDto, bmhkLaboratorium.getId().toString());
	}

	private static BMHKLaboratorium getBmhkLaboratorium(CervixMonster monster, BMHKLaboratorium bmhkLaboratorium)
	{
		if (bmhkLaboratorium == null)
		{
			bmhkLaboratorium = monster.getLaboratorium();
		}
		return bmhkLaboratorium;
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
