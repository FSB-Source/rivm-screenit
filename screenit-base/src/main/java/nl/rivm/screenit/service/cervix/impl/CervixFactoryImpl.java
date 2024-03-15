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
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvAnalyseresultaten;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvAnalyseresultaat;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvOrderCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
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
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixFactoryImpl implements CervixFactory
{
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
	private CervixMonsterDao monsterDao;

	@Autowired
	private Cervix2023StartBepalingService bmhk2023startBepalingService;

	@Override
	public CervixScreeningRonde maakRonde(CervixDossier dossier, boolean setDatumVolgendeRonde)
	{
		return maakRonde(dossier, dateSupplier.getLocalDateTime(), setDatumVolgendeRonde);
	}

	@Override
	public CervixScreeningRonde maakRonde(CervixDossier dossier)
	{
		return maakRonde(dossier, dateSupplier.getLocalDateTime(), true);
	}

	@Override
	public CervixScreeningRonde maakRonde(CervixDossier dossier, LocalDateTime creatiedatum, boolean setDatumVolgendeRonde)
	{
		LOG.info("CervixScreeningRonde aanmaken voor client (id: '{}')", dossier.getClient().getId());

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

		if (setDatumVolgendeRonde)
		{
			updateDossierMetVolgendeRondeDatum(dossier, creatiedatum);
		}
		hibernateService.saveOrUpdate(ronde);
		hibernateService.saveOrUpdate(dossier);
		return ronde;
	}

	@Override
	public void updateDossierMetVolgendeRondeDatum(CervixDossier dossier, LocalDateTime creatiedatum)
	{
		LocalDate geboortedatum = DateUtil.toLocalDate(dossier.getClient().getPersoon().getGeboortedatum());
		CervixLeeftijdcategorie leeftijdcategorie = CervixLeeftijdcategorie.getLeeftijdcategorie(geboortedatum, creatiedatum);
		LocalDate volgendeRondeVanaf = geboortedatum.plusYears(leeftijdcategorie.volgende().getLeeftijd());

		if (geboortedatum.getDayOfMonth() != volgendeRondeVanaf.getDayOfMonth())
		{

			volgendeRondeVanaf = volgendeRondeVanaf.plusDays(1);
		}
		dossier.setVolgendeRondeVanaf(DateUtil.toUtilDate(volgendeRondeVanaf));
		hibernateService.saveOrUpdate(dossier);
	}

	@Override
	public CervixUitnodiging maakHeraanvraagUitnodiging(CervixScreeningRonde ronde, CervixBrief brief)
	{
		return maakUitnodigingMetVoorEnNaBmhk2023HerinnerenCheck(ronde, brief, false);
	}

	@Override
	public CervixUitnodiging maakUitnodigingMetVoorEnNaBmhk2023HerinnerenCheck(CervixScreeningRonde ronde, CervixBrief brief, boolean herinnerenPreBmhk2023)
	{
		var herinneren = herinnerenPreBmhk2023;
		if (bmhk2023startBepalingService.rondeValtBinnenBmhk2023(ronde))
		{
			var herinnerenNaBmhk2023 = moetBriefHerinnertWordenPostBmhk2023(ronde, brief.getBriefType());
			herinneren |= herinnerenNaBmhk2023;
		}

		return maakUitnodiging(ronde, brief, herinneren, true);
	}

	private boolean moetBriefHerinnertWordenPostBmhk2023(CervixScreeningRonde ronde, BriefType briefType)
	{
		return BriefType.getCervixUitnodigingen().contains(briefType)
			&& !screeningrondeService.clientHeeftAanOnderzoekMeegedaanInRonde(ronde);
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, BriefType briefType)
	{
		return maakUitnodiging(ronde, briefType, false, true);
	}

	@Override
	public void maakVooraankondiging(CervixScreeningRonde ronde)
	{
		var dossier = ronde.getDossier();
		if (dossier.getVooraankondigingsBrief() == null)
		{
			LOG.info("CervixVooraankondigingbrief aanmaken client (id: '{}')", dossier.getClient().getId());
			var brief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_VOORAANKONDIGING);
			dossier.setVooraankondigingsBrief(brief);
			hibernateService.saveOrUpdate(dossier);
		}
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, CervixBrief brief)
	{
		return maakUitnodiging(ronde, brief, false, true);
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, BriefType briefType, boolean herinneren, boolean herinneringOnderbreken)
	{
		CervixBrief brief = briefService.maakBvoBrief(ronde, briefType);
		return maakUitnodiging(ronde, brief, herinneren, herinneringOnderbreken);
	}

	@Override
	public CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, CervixBrief brief, boolean herinneren, boolean herinneringOnderbreken)
	{
		LOG.info("CervixUitnodiging aanmaken client (id: '{}')", ronde.getDossier().getClient().getId());

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
		uitnodiging.setUitnodigingsDatum(nu);

		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(uitnodiging);
		hibernateService.saveOrUpdate(ronde);
		if (brief.getBriefType() == BriefType.CERVIX_UITNODIGING_30)
		{
			maakGecombineerdeUitnodiging(uitnodiging);
		}
		return uitnodiging;
	}

	private void maakGecombineerdeUitnodiging(CervixUitnodiging uitstrijkjeUitnodiging)
	{
		if (uitstrijkjeUitnodiging.getGecombineerdeUitstrijkje() != null || uitstrijkjeUitnodiging.getGecombineerdeZas() != null
			|| uitstrijkjeUitnodiging.getMonsterType() != CervixMonsterType.UITSTRIJKJE)
		{
			throw new IllegalStateException("uitnodiging moet een uitstrijkje zijn en er mogen nog geen gecombineerde uitnodigingen gekoppeld zijn");
		}
		if (bmhk2023startBepalingService.rondeValtBinnenBmhk2023(uitstrijkjeUitnodiging.getScreeningRonde()))
		{
			var ronde = uitstrijkjeUitnodiging.getScreeningRonde();
			var zasUitnodiging = maakZasUitnodiging(ronde.getDossier().getClient(), BriefType.CERVIX_ZAS_COMBI_UITNODIGING_30, null, false, false);
			zasUitnodiging.setGecombineerdeUitstrijkje(uitstrijkjeUitnodiging);
			uitstrijkjeUitnodiging.setGecombineerdeZas(zasUitnodiging);
			zasUitnodiging.setAangevraagdeHerdruk(uitstrijkjeUitnodiging.getAangevraagdeHerdruk());
			hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(uitstrijkjeUitnodiging.getBrief(), true));
			hibernateService.saveOrUpdateAll(zasUitnodiging, uitstrijkjeUitnodiging);
		}
	}

	@Override
	public CervixUitnodiging maakZasUitnodiging(Client client, Account account, boolean uitstelDatumNemen, boolean zasAangevraagdDoorClient)
	{
		return maakZasUitnodiging(client, BriefType.CERVIX_ZAS_UITNODIGING, account, uitstelDatumNemen, zasAangevraagdDoorClient);
	}

	@Override
	public CervixUitnodiging maakHerinneringNonResponderMetZas(Client client)
	{
		var uitnodiging = maakZasUitnodiging(client, BriefType.CERVIX_ZAS_NON_RESPONDER, null, false, false);
		uitnodiging.setHerinneren(true);
		hibernateService.saveOrUpdate(uitnodiging);

		return uitnodiging;
	}

	private CervixUitnodiging maakZasUitnodiging(Client client, BriefType zasBriefType, Account account, boolean uitstelDatumNemen, boolean zasAangevraagdDoorClient)
	{
		var laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
		var laatsteZasUitnodiging = laatsteScreeningRonde.getLaatsteZasUitnodiging();
		var laatsteUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();

		if (laatsteZasUitnodiging != null && laatsteZasUitnodiging.getGeannuleerdDatum() == null && laatsteZasUitnodiging.getVerstuurdDatum() == null)
		{
			laatsteZasUitnodiging.setGeannuleerdDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(laatsteZasUitnodiging);
		}
		if (laatsteUitnodiging != null && laatsteUitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var brief = laatsteUitnodiging.getBrief();
			if (BriefUtil.getMergedBrieven(brief) == null)
			{
				if (heeftEersteUitnodigingOntvangen(laatsteScreeningRonde))
				{
					hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
				}
			}
		}
		var nieuweZasUitnodiging = maakUitnodiging(laatsteScreeningRonde, zasBriefType, uitnodigingKrijgtHerinnering(laatsteUitnodiging), true);
		nieuweZasUitnodiging.setZasAangevraagdDoorClient(zasAangevraagdDoorClient);
		var uitstel = laatsteScreeningRonde.getUitstel();
		if (uitstel != null && uitstel.getGeannuleerdDatum() == null)
		{
			if (uitstelDatumNemen)
			{
				nieuweZasUitnodiging.setUitnodigingsDatum(uitstel.getUitstellenTotDatum());
			}
			screeningrondeService.annuleerUitstel(laatsteScreeningRonde);
		}
		hibernateService.saveOrUpdate(nieuweZasUitnodiging);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_AANGEVRAAGD, account, client, Bevolkingsonderzoek.CERVIX);

		return nieuweZasUitnodiging;
	}

	private boolean uitnodigingKrijgtHerinnering(CervixUitnodiging laatsteUitnodiging)
	{
		if (laatsteUitnodiging != null)
		{
			if (BriefType.CERVIX_ZAS_NON_RESPONDER == laatsteUitnodiging.getBrief().getBriefType())
			{
				return false;
			}

			return laatsteUitnodiging.getHerinneren();
		}
		return true;
	}

	private boolean heeftEersteUitnodigingOntvangen(CervixScreeningRonde ronde)
	{
		return ronde.getUitnodigingen().stream()
			.anyMatch(uitnodiging -> BriefType.getCervixUitnodigingen().contains(uitnodiging.getBrief().getBriefType())
				&& BriefUtil.isMergedBrievenGeprint(uitnodiging.getBrief()));
	}

	private CervixUitstrijkje maakUitstrijkje(CervixUitnodiging uitnodiging)
	{
		Long monsterIdLong = monsterDao.getNextMonsterId();

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

		LOG.info("CervixUitstrijkje aanmaken client (id: '{}'), monster (id: '{}'), monsterControleLetters: {}", cervixDossier.getClient().getId(), uitstrijkje.getId(),
			monsterControleLetters);

		return uitstrijkje;
	}

	@Override
	public CervixZas maakZasMonster(CervixUitnodiging uitnodiging, String monsterId)
	{
		LOG.info("ZAS monster met monsterId '{}' aanmaken voor uitnodiging (uitnodigingId: '{}') van client (id: '{}')", monsterId, uitnodiging.getUitnodigingsId(),
			uitnodiging.getScreeningRonde().getDossier().getClient().getId());

		CervixZas zas = new CervixZas();
		zas.setZasStatus(CervixZasStatus.VERSTUURD);
		zas.setStatusDatum(dateSupplier.getDate());
		zas.setMonsterId(monsterId);
		zas.setUitnodiging(uitnodiging);
		uitnodiging.setMonster(zas);

		hibernateService.saveOrUpdate(zas);
		hibernateService.saveOrUpdate(uitnodiging);
		voegGecombineerdeBriefToeAanMergedBrievenVanZas(uitnodiging);
		return zas;
	}

	@Override
	public CervixHpvBericht maakHpvBericht(BMHKLaboratorium laboratorium, String instrumentId, String hl7Bericht, String messageId)
	{
		LOG.info("CervixHpvBericht aanmaken voor laboratorium: {} met messageId: '{}'", laboratorium.getNaam(), messageId);

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
	public CervixHpvBeoordeling maakHpvBeoordeling(CervixMonster monster, CervixHpvBericht hpvBericht, Date analyseDatum, Date autorisatieDatum,
		CervixHpvBeoordelingWaarde hpvUitslag,
		List<CervixHpvAnalyseresultaat> analyseresultaten)
	{
		LOG.info("CervixHpvBeoordeling aanmaken voor client (id: '{}') met monster (id: '{}')", monster.getUitnodiging().getScreeningRonde().getDossier().getClient().getId(),
			monster.getId());

		CervixHpvBeoordeling hpvBeoordeling = new CervixHpvBeoordeling();
		hpvBeoordeling.setAnalyseDatum(analyseDatum);
		hpvBeoordeling.setAutorisatieDatum(autorisatieDatum);
		hpvBeoordeling.setHpvUitslag(hpvUitslag);
		hpvBeoordeling.setHpvBericht(hpvBericht);
		hpvBeoordeling.setMonster(monster);

		monster.setLaatsteHpvBeoordeling(hpvBeoordeling);
		monster.getHpvBeoordelingen().add(hpvBeoordeling);

		hibernateService.saveOrUpdate(hpvBeoordeling);
		saveAnalyseresultaten(analyseresultaten, hpvBeoordeling);
		hibernateService.saveOrUpdate(monster);
		return hpvBeoordeling;
	}

	private void saveAnalyseresultaten(List<CervixHpvAnalyseresultaat> analyseresultaten, CervixHpvBeoordeling hpvBeoordeling)
	{
		if (analyseresultaten != null)
		{
			CervixHpvAnalyseresultaten persistentAnalyseresultaten = new CervixHpvAnalyseresultaten();
			hpvBeoordeling.setAnalyseresultaten(persistentAnalyseresultaten);
			persistentAnalyseresultaten.setBeoordeling(hpvBeoordeling);
			persistentAnalyseresultaten.setHpvhr(getResultValue(analyseresultaten, CervixHpvOrderCode.PAN, CervixHpvResultCode.HR));
			persistentAnalyseresultaten.setHpvohr(getResultValue(analyseresultaten, CervixHpvOrderCode.GEN, CervixHpvResultCode.OHR));
			persistentAnalyseresultaten.setHpv16(getResultValue(analyseresultaten, CervixHpvOrderCode.GEN, CervixHpvResultCode.HPV16));
			persistentAnalyseresultaten.setHpv18(getResultValue(analyseresultaten, CervixHpvOrderCode.GEN, CervixHpvResultCode.HPV18));
			hibernateService.saveOrUpdate(hpvBeoordeling);
			hibernateService.saveOrUpdate(persistentAnalyseresultaten);
		}
	}

	private static CervixHpvResultValue getResultValue(List<CervixHpvAnalyseresultaat> analyseresultaten, CervixHpvOrderCode orderCode, CervixHpvResultCode resultCode)
	{
		return analyseresultaten.stream().filter(ar -> ar.getOrderCode().equals(orderCode) && ar.getResultCode().equals(
			resultCode)).findFirst().orElse(new CervixHpvAnalyseresultaat()).getResultValue();
	}

	@Override
	public CervixCytologieOrder maakCytologieOrder(CervixUitstrijkje uitstrijkje, CervixCytologieReden cytologieReden, String hl7Bericht)
	{
		LOG.info("CervixCytologieOrder aanmaken voor client (id: '{}') met monster (id: '{}')", uitstrijkje.getUitnodiging().getScreeningRonde().getDossier().getClient().getId(),
			uitstrijkje.getId());

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
		hibernateService.saveOrUpdateAll(uitstel, ronde);
		return uitstel;
	}

	private void voegGecombineerdeBriefToeAanMergedBrievenVanZas(CervixUitnodiging zasUitnodiging)
	{
		var gecombineerdeUitstrijkjeUitnodiging = zasUitnodiging.getGecombineerdeUitstrijkje();
		if (gecombineerdeUitstrijkjeUitnodiging != null)
		{
			var gecombineerdeUitstrijkjeUitnodigingBrief = gecombineerdeUitstrijkjeUitnodiging.getBrief();
			var mergedBrieven = zasUitnodiging.getBrief().getMergedBrieven();
			mergedBrieven.setPrintDatum(dateSupplier.getDate());
			gecombineerdeUitstrijkjeUitnodigingBrief.setMergedBrieven(mergedBrieven);
			gecombineerdeUitstrijkjeUitnodigingBrief.setGegenereerd(true);

			hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(gecombineerdeUitstrijkjeUitnodigingBrief, false));
			hibernateService.saveOrUpdate(mergedBrieven);
		}
	}
}
