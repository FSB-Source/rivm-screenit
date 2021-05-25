package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixMonsterService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixFactoryImpl implements CervixFactory
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixFactoryImpl.class);

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private CervixBaseScreeningrondeService screeningrondeService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private CervixMonsterService monsterService;

	@Override
	public CervixScreeningRonde maakRonde(CervixDossier dossier)
	{
		return maakRonde(dossier, dateSupplier.getLocalDateTime());
	}

	@Override
	public CervixScreeningRonde maakRonde(CervixDossier dossier, LocalDateTime creatiedatum)
	{
		LOG.info("CervixScreeningRonde aanmaken voor clientId " + dossier.getClient().getId());

		CervixScreeningRonde vorigeRonde = dossier.getLaatsteScreeningRonde();
		if (vorigeRonde != null && vorigeRonde.getStatus() == ScreeningRondeStatus.LOPEND)
		{
			vorigeRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			vorigeRonde.setStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(vorigeRonde);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_RONDE_VERLOPEN, dossier.getClient(), Bevolkingsonderzoek.CERVIX);
		}

		CervixScreeningRonde ronde = new CervixScreeningRonde();
		ronde.setStatus(ScreeningRondeStatus.LOPEND);
		ronde.setStatusDatum(dateSupplier.getDate());
		ronde.setCreatieDatum(DateUtil.toUtilDate(creatiedatum));
		ronde.setAangemeld(true);
		ronde.setDossier(dossier);
		LocalDate geboortedatum = DateUtil.toLocalDate(dossier.getClient().getPersoon().getGeboortedatum());
		CervixLeeftijdcategorie leeftijdcategorie = CervixLeeftijdcategorie.getLeeftijdcategorie(geboortedatum, creatiedatum);
		ronde.setLeeftijdcategorie(leeftijdcategorie);

		dossier.getScreeningRondes().add(ronde);
		dossier.setLaatsteScreeningRonde(ronde);

		LocalDate volgendeRondeVanaf = geboortedatum.plusYears(leeftijdcategorie.volgende().getLeeftijd());
		if (geboortedatum.getDayOfMonth() != volgendeRondeVanaf.getDayOfMonth())
		{

			volgendeRondeVanaf = volgendeRondeVanaf.plusDays(1);
		}
		dossier.setVolgendeRondeVanaf(DateUtil.toUtilDate(volgendeRondeVanaf));

		hibernateService.saveOrUpdate(ronde);
		hibernateService.saveOrUpdate(dossier);
		return ronde;
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, BriefType briefType)
	{
		return maakUitnodiging(ronde, briefType, false, true);
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, CervixBrief brief)
	{
		return maakUitnodiging(ronde, brief, false, true);
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, BriefType briefType, boolean herinneren, boolean herinneringOnderbreken)
	{
		CervixBrief brief = briefService.maakCervixBrief(ronde, briefType);
		return maakUitnodiging(ronde, brief, herinneren, herinneringOnderbreken);
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, CervixBrief brief, boolean herinneren, boolean herinneringOnderbreken)
	{
		LOG.info("CervixUitnodiging aanmaken voor clientId " + ronde.getDossier().getClient().getId());

		if (herinneringOnderbreken)
		{
			screeningrondeService.annuleerHerinnering(ronde);
		}

		CervixMonsterType monsterType = CervixMonsterType.getMonsterType(brief.getBriefType());

		CervixUitnodiging uitnodiging = new CervixUitnodiging();
		uitnodiging.setAangevraagdeHerdruk(brief.isAangevraagdeHerdruk());
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		uitnodiging.setMonsterType(monsterType);
		Date nu = dateSupplier.getDate();
		uitnodiging.setCreatieDatum(nu);
		uitnodiging.setHerinnering(false);
		uitnodiging.setUitgesteld(false);

		uitnodiging.setScreeningRonde(ronde);
		ronde.getUitnodigingen().add(uitnodiging);
		if (ronde.getEersteUitnodiging() == null)
		{
			ronde.setEersteUitnodiging(uitnodiging);
		}
		ronde.setLaatsteUitnodiging(uitnodiging);
		uitnodiging.setBrief(brief);
		brief.setUitnodiging(uitnodiging);
		if (ronde.getEersteUitnodiging() == null)
		{
			ronde.setEersteUitnodiging(uitnodiging);
		}

		Date uitnodigsdatum = nu;
		switch (monsterType)
		{
		case UITSTRIJKJE:
			uitnodiging.setHerinneren(herinneren);
			maakUitstrijkje(uitnodiging);
			break;
		case ZAS:
			uitnodiging.setHerinneren(herinneren);
			hibernateService.saveOrUpdate(uitnodiging);
			ronde.setLaatsteZasUitnodiging(uitnodiging);
			break;
		default:
			throw new IllegalStateException();
		}
		uitnodiging.setUitnodigingsDatum(uitnodigsdatum);

		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(uitnodiging);
		hibernateService.saveOrUpdate(ronde);
		return uitnodiging;
	}

	@Override
	public CervixUitnodiging maakZasUitnodiging(Client client, Account account, boolean uitstelDatumNemen, boolean zasAangevraagdDoorClient)
	{
		CervixScreeningRonde laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
		CervixUitnodiging laatsteZasUitnodiging = laatsteScreeningRonde.getLaatsteZasUitnodiging();
		CervixUitnodiging laatsteUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();

		if (laatsteZasUitnodiging != null && laatsteZasUitnodiging.getGeannuleerdDatum() == null && laatsteZasUitnodiging.getVerstuurdDatum() == null)
		{
			laatsteZasUitnodiging.setGeannuleerdDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(laatsteZasUitnodiging);
		}
		if (laatsteUitnodiging != null && laatsteUitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			CervixBrief brief = laatsteUitnodiging.getBrief();
			if (brief.getMergedBrieven() == null)
			{
				if (!screeningrondeService.isFrisseStart(laatsteScreeningRonde) && heeftEersteUitnodingOntvangen(laatsteScreeningRonde))
				{
					brief.setTegenhouden(true);
				}
				hibernateService.saveOrUpdate(brief);
			}
		}
		CervixUitnodiging uitnodiging = maakUitnodiging(laatsteScreeningRonde, BriefType.CERVIX_ZAS_UITNODIGING, laatsteUitnodiging.getHerinneren(), true);
		uitnodiging.setZasAangevraagdDoorClient(zasAangevraagdDoorClient);
		CervixUitstel uitstel = laatsteScreeningRonde.getUitstel();
		if (uitstel != null && uitstel.getGeannuleerdDatum() == null)
		{
			if (uitstelDatumNemen)
			{
				uitnodiging.setUitnodigingsDatum(uitstel.getUitstellenTotDatum());
			}
			screeningrondeService.annuleerUitstel(laatsteScreeningRonde);
			hibernateService.saveOrUpdate(uitstel);
		}
		hibernateService.saveOrUpdate(uitnodiging);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_AANGEVRAAGD, account, client, Bevolkingsonderzoek.CERVIX);

		return uitnodiging;
	}

	private boolean heeftEersteUitnodingOntvangen(CervixScreeningRonde ronde)
	{
		return ronde.getUitnodigingen().stream()
			.anyMatch(uitnodiging -> BriefType.CERVIX_UITNODIGING.equals(uitnodiging.getBrief().getBriefType())
				&& uitnodiging.getBrief().getMergedBrieven() != null
				&& uitnodiging.getBrief().getMergedBrieven().getGeprint());
	}

	private CervixUitstrijkje maakUitstrijkje(CervixUitnodiging uitnodiging)
	{
		Long monsterIdLong = monsterService.getNextMonsterId();

		String monsterId = monsterIdLong < 100 ? String.format("%010d", monsterIdLong) : monsterIdLong.toString();

		CervixUitstrijkje uitstrijkje = new CervixUitstrijkje();
		uitnodiging.setMonster(uitstrijkje);
		hibernateService.saveOrUpdate(uitnodiging);

		uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.NIET_ONTVANGEN);
		uitstrijkje.setStatusDatum(dateSupplier.getDate());
		uitstrijkje.setMonsterId(monsterId);
		uitstrijkje.setUitnodiging(uitnodiging);

		CervixDossier cervixDossier = uitnodiging.getScreeningRonde().getDossier();
		String monsterControleLetters = CervixMonsterUtil.getMonsterControleLetters(cervixDossier);
		uitstrijkje.setControleLetters(monsterControleLetters);

		hibernateService.saveOrUpdate(uitstrijkje);

		LOG.info("CervixUitstrijkje aanmaken voor client met id "
			+ cervixDossier.getClient().getId()
			+ " met monsterId " + monsterId + " en monsterControleLetters " + monsterControleLetters);

		return uitstrijkje;
	}

	@Override
	public CervixZas maakZas(CervixUitnodiging uitnodiging, String monsterId)
	{
		LOG.info("CervixZas aanmaken voor clientId " + uitnodiging.getScreeningRonde().getDossier().getClient().getId() + " met uitnodigingId "
			+ uitnodiging.getUitnodigingsId());

		CervixZas zas = new CervixZas();
		zas.setZasStatus(CervixZasStatus.VERSTUURD);
		zas.setStatusDatum(dateSupplier.getDate());
		zas.setMonsterId(monsterId);
		zas.setUitnodiging(uitnodiging);
		uitnodiging.setMonster(zas);

		hibernateService.saveOrUpdate(zas);
		hibernateService.saveOrUpdate(uitnodiging);
		return zas;
	}

	@Override
	public CervixHpvBericht maakHpvBericht(BMHKLaboratorium laboratorium, String instrumentId, String hl7Bericht, String messageId)
	{
		LOG.info("CervixHpvBericht aanmaken voor laboratorium: " + laboratorium.getNaam() + " met messageId " + messageId);

		CervixHpvBericht hpvBericht = new CervixHpvBericht();
		hpvBericht.setStatus(BerichtStatus.NIEUW);
		hpvBericht.setStatusDatum(dateSupplier.getDate());
		hpvBericht.setOntvangen(dateSupplier.getDate());
		hpvBericht.setLaboratorium(laboratorium);
		hpvBericht.setInstrumentId(instrumentId);
		hpvBericht.setHl7Bericht(hl7Bericht);
		hpvBericht.setMessageId(messageId);

		hibernateService.saveOrUpdate(hpvBericht);
		return hpvBericht;
	}

	@Override
	public CervixHpvBeoordeling maakHpvBeoordeling(CervixMonster monster, CervixHpvBericht hpvBericht, Date analyseDatum, Date autorisatieDatum, CervixHpvUitslag hpvUitslag)
	{
		LOG.info("CervixHpvBeoordeling aanmaken voor clientId " + monster.getUitnodiging().getScreeningRonde().getDossier().getClient().getId() + " met monsterId "
			+ monster.getMonsterId());

		CervixHpvBeoordeling hpvBeoordeling = new CervixHpvBeoordeling();
		hpvBeoordeling.setAnalyseDatum(analyseDatum);
		hpvBeoordeling.setAutorisatieDatum(autorisatieDatum);
		hpvBeoordeling.setHpvUitslag(hpvUitslag);
		hpvBeoordeling.setHpvBericht(hpvBericht);
		hpvBeoordeling.setMonster(monster);

		monster.setLaatsteHpvBeoordeling(hpvBeoordeling);
		monster.getHpvBeoordelingen().add(hpvBeoordeling);

		hibernateService.saveOrUpdate(hpvBeoordeling);
		hibernateService.saveOrUpdate(monster);
		return hpvBeoordeling;
	}

	@Override
	public CervixCytologieOrder maakCytologieOrder(CervixUitstrijkje uitstrijkje, CervixCytologieReden cytologieReden, String hl7Bericht)
	{
		LOG.info("CervixCytologieOrder aanmaken voor clientId " + uitstrijkje.getUitnodiging().getScreeningRonde().getDossier().getClient().getId() + " met monsterId "
			+ uitstrijkje.getMonsterId());

		CervixCytologieOrder cytologieOrder = new CervixCytologieOrder();
		cytologieOrder.setUitstrijkje(uitstrijkje);
		cytologieOrder.setStatus(CervixCytologieOrderStatus.AANGEMAAKT);
		cytologieOrder.setStatusDatum(dateSupplier.getDate());
		cytologieOrder.setCytologieReden(cytologieReden);
		cytologieOrder.setHl7Bericht(hl7Bericht);
		cytologieOrder.setUitstrijkje(uitstrijkje);
		uitstrijkje.setCytologieOrder(cytologieOrder);

		hibernateService.saveOrUpdate(uitstrijkje);
		hibernateService.saveOrUpdate(cytologieOrder);

		return cytologieOrder;
	}

	@Override
	public CervixUitstel maakUitstel(CervixScreeningRonde ronde, Date uitstellenTotDatum, CervixUitstelType uitstelType)
	{
		CervixUitstel uitstel = new CervixUitstel();
		uitstel.setScreeningRonde(ronde);
		uitstel.setUitstellenTotDatum(uitstellenTotDatum);
		uitstel.setUitstelType(uitstelType);
		uitstel.setWijzigingsDatum(dateSupplier.getDate());
		ronde.setUitstel(uitstel);
		hibernateService.saveOrUpdate(uitstel);
		hibernateService.saveOrUpdate(ronde);
		return uitstel;
	}
}
