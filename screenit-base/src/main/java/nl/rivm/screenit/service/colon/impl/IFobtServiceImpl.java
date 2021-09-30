package nl.rivm.screenit.service.colon.impl;

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

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.colon.IFobtDao;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.colon.ScannedAntwoordFormulier;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class IFobtServiceImpl implements IFobtService
{
	private static final Logger LOG = LoggerFactory.getLogger(IFobtServiceImpl.class);

	@Autowired
	private IFobtDao iFobtDao;

	@Autowired
	private BaseHoudbaarheidService houdbaarheidService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private ColonScreeningsrondeService screeningsrondeService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	@Autowired
	private ColonStudietestService studietestService;

	@Override
	public IFOBTTest getIfobtTest(String barcode)
	{
		return iFobtDao.getIfobtTest(barcode);
	}

	private void rondeSluitenIndienMogelijk(LocalDateTime nu, ColonScreeningRonde ronde)
	{
		boolean allesAfgerondEnGunstig = true;
		boolean isUitslagBriefVerstuurd = false;
		for (IFOBTTest ifobtTest : ronde.getIfobtTesten())
		{
			if (IFOBTTestStatus.ACTIEF.equals(ifobtTest.getStatus()) || IFOBTTestUtil.isOngunstig(ifobtTest))
			{
				allesAfgerondEnGunstig = false;
				break;
			}
		}

		isUitslagBriefVerstuurd = ColonScreeningRondeUtil.heeftUitslagBrief(ronde);

		if (allesAfgerondEnGunstig && isUitslagBriefVerstuurd)
		{
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS)));
		}
		ColonUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null && !laatsteUitnodiging.isVerstuurd())
		{
			ronde.getUitnodigingen().remove(laatsteUitnodiging);
			ronde.setLaatsteUitnodiging(null);
			hibernateService.delete(laatsteUitnodiging);

			ronde.setLaatsteUitnodiging(ronde.getUitnodigingen().stream().max(Comparator.comparing(ColonUitnodiging::getUitnodigingsId)).orElse(null));
		}

	}

	private void setNormWaarde(IFOBTTest buis, BigDecimal normWaarde)
	{
		if (buis.getStatus() != IFOBTTestStatus.NIETTEBEOORDELEN)
		{
			buis.setNormWaarde(normWaarde);
		}
		if (buis.getStatus() != IFOBTTestStatus.DOETNIETMEE && buis.getStatus() != IFOBTTestStatus.VERWIJDERD)
		{
			checkVervaldatumVerlopen(buis);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void uitslagFitOntvangen(IFOBTTest fitMetUitslag)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("uitslagOntvangen " + fitMetUitslag.getBarcode());
		}

		ColonDossier dossier = fitMetUitslag.getColonScreeningRonde().getDossier();
		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(dossier.getClient(), currentDateSupplier.getDate());
		BigDecimal normWaardeGold = getIFOBTNormWaarde(projectClient);
		ColonUitnodiging uitnodiging = IFOBTTestUtil.getUitnodiging(fitMetUitslag);

		if (!fitMetUitslag.getColonScreeningRonde().equals(dossier.getLaatsteScreeningRonde()))
		{
			studietestService.projectClientInactiverenBijVergelijkendOnderzoek(fitMetUitslag.getColonScreeningRonde());
		}
		uitnodigingService.berekenEnSetUitgesteldeUitslagDatum(uitnodiging);

		ColonScreeningRonde screeningRonde = uitslagNaarJuisteRonde(fitMetUitslag);
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		setNormWaarde(fitMetUitslag, normWaardeGold);
		bepaalEnSetHeraanmeldenTekstKey(fitMetUitslag);
		heraanmelden(screeningRonde, nu);
		boolean briefGemaakt = maakBuitenDoelgroepBriefIndienNodig(screeningRonde, fitMetUitslag);
		if (fitMetUitslag.getStatus() != IFOBTTestStatus.VERVALDATUMVERLOPEN)
		{
			if (IFOBTTestUtil.isOngunstig(fitMetUitslag) || fitMetUitslag.getStatus() != IFOBTTestStatus.VERWIJDERD)
			{
				setStatus(uitnodiging, IFOBTTestStatus.UITGEVOERD);
			}
			if (!briefGemaakt)
			{
				alsClientEerderUitslagHeeftGehadDanExtraUitslagBriefMaken(fitMetUitslag, screeningRonde);
			}
			rondeSluitenIndienMogelijk(nu, screeningRonde);
			saveOrUpdateBuis(uitnodiging);
		}
		else
		{
			saveOrUpdateBuis(uitnodiging);
			if (!briefGemaakt)
			{
				ColonUitnodiging nieuweUitnodiging = screeningsrondeService.createNieuweUitnodiging(screeningRonde, ColonUitnodigingCategorie.U6);
				alsGeenPakketGemaaktDanExtraUitslagBriefMaken(fitMetUitslag, screeningRonde, nieuweUitnodiging);
			}
		}
		hibernateService.saveOrUpdateAll(uitnodiging, screeningRonde);
	}

	private void alsGeenPakketGemaaktDanExtraUitslagBriefMaken(IFOBTTest test, ColonScreeningRonde screeningRonde, ColonUitnodiging nieuweUitnodiging)
	{
		if (nieuweUitnodiging == null
			&& (ColonScreeningRondeUtil.getEersteGunstigeTest(screeningRonde) != null || ColonScreeningRondeUtil.getEersteOngunstigeTest(screeningRonde) != null))
		{
			maakUitslagBriefExtraMonster(screeningRonde, test);
		}
	}

	private void alsClientEerderUitslagHeeftGehadDanExtraUitslagBriefMaken(IFOBTTest nieuweBuisMetUitslag, ColonScreeningRonde ronde)
	{

		boolean heeftEerderUitslagGehad = ronde.getOpenUitnodiging() != null;
		IFOBTTest buisVoorBrief = nieuweBuisMetUitslag;
		if (!heeftEerderUitslagGehad)
		{
			List<IFOBTTest> testen = new ArrayList<>(ronde.getIfobtTesten());
			testen.remove(nieuweBuisMetUitslag);
			if (testen.size() > 0)
			{
				boolean hadGunstigeUitslag = false;
				boolean hadOngunstigeUitslag = false;

				for (IFOBTTest test : testen)
				{
					if (test.getStatus() == IFOBTTestStatus.UITGEVOERD)
					{
						if (IFOBTTestUtil.isGunstig(test) && test.getType().equals(IFOBTType.GOLD))
						{
							hadGunstigeUitslag = true;
						}
						if (IFOBTTestUtil.isOngunstig(test))
						{
							hadOngunstigeUitslag = true;
						}
					}
				}

				if (hadOngunstigeUitslag || hadGunstigeUitslag)
				{

					heeftEerderUitslagGehad = hadGunstigeUitslag && hadOngunstigeUitslag;
					if (!heeftEerderUitslagGehad)
					{

						heeftEerderUitslagGehad = hadGunstigeUitslag && IFOBTTestUtil.isGunstig(nieuweBuisMetUitslag)
							|| hadOngunstigeUitslag && IFOBTTestUtil.isOngunstig(nieuweBuisMetUitslag);
					}
					if (!heeftEerderUitslagGehad)
					{

						boolean hadOngunstigeUitslagBrief = hadOngunstigeUitslag
							&& ronde.getBrieven().stream().anyMatch(b -> b.getBriefType() == BriefType.COLON_UITNODIGING_INTAKE);
						heeftEerderUitslagGehad = hadOngunstigeUitslagBrief;
					}
					if (!heeftEerderUitslagGehad)
					{

						boolean hadGunstigeUitslagZonderBrief = hadGunstigeUitslag && IFOBTTestUtil.isOngunstig(nieuweBuisMetUitslag)
							&& ronde.getBrieven().stream().noneMatch(b -> b.getBriefType() == BriefType.COLON_GUNSTIGE_UITSLAG);
						heeftEerderUitslagGehad = hadGunstigeUitslagZonderBrief;
						buisVoorBrief = ColonScreeningRondeUtil.getEersteGunstigeTest(ronde);
					}
				}
			}
		}
		if (heeftEerderUitslagGehad)
		{

			maakUitslagBriefExtraMonster(ronde, buisVoorBrief);
		}
	}

	private boolean maakBuitenDoelgroepBriefIndienNodig(ColonScreeningRonde ronde, IFOBTTest buisVoorBrief)
	{
		boolean briefGemaakt = false;

		if (screeningsrondeService.isRondeStatusBuitenDoelgroep(ronde))
		{
			if (IFOBTTestUtil.isOngunstig(buisVoorBrief))
			{
				maakBriefEnKoppelAanTest(ronde, buisVoorBrief, BriefType.COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP);
				briefGemaakt = true;
			}
			else if (buisVoorBrief.getStatus() == IFOBTTestStatus.VERVALDATUMVERLOPEN || buisVoorBrief.getStatus() == IFOBTTestStatus.NIETTEBEOORDELEN)
			{
				maakBriefEnKoppelAanTest(ronde, buisVoorBrief, BriefType.COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP);
				briefGemaakt = true;
			}
		}
		return briefGemaakt;
	}

	private void maakUitslagBriefExtraMonster(ColonScreeningRonde ronde, IFOBTTest buisVoorBrief)
	{
		maakBriefEnKoppelAanTest(ronde, buisVoorBrief, BriefType.COLON_UITSLAGBRIEF_EXTRA_MONSTER);
	}

	private void maakBriefEnKoppelAanTest(ColonScreeningRonde ronde, IFOBTTest buisVoorBrief, BriefType briefType)
	{
		ColonBrief brief = briefService.maakColonBrief(ronde, briefType);
		brief.setIfobtTest(buisVoorBrief);
		hibernateService.saveOrUpdate(brief);
	}

	private ColonScreeningRonde uitslagNaarJuisteRonde(IFOBTTest buisMetUitslag)
	{
		ColonUitnodiging uitnodiging = buisMetUitslag.getColonUitnodiging();
		ColonScreeningRonde screeningRonde = uitnodiging.getScreeningRonde();
		ColonScreeningRonde laatsteScreeningRonde = screeningRonde.getDossier().getLaatsteScreeningRonde();
		if (!screeningRonde.equals(laatsteScreeningRonde))
		{

			screeningRonde.getIfobtTesten().remove(buisMetUitslag);
			laatsteScreeningRonde.getIfobtTesten().add(buisMetUitslag);
			buisMetUitslag.setColonScreeningRonde(laatsteScreeningRonde);
			IFOBTTest gekoppeldeExtraTest = uitnodiging.getGekoppeldeExtraTest();
			if (gekoppeldeExtraTest != null)
			{
				screeningRonde.getIfobtTesten().remove(gekoppeldeExtraTest);
				laatsteScreeningRonde.getIfobtTesten().add(gekoppeldeExtraTest);
				gekoppeldeExtraTest.setColonScreeningRonde(laatsteScreeningRonde);
				hibernateService.saveOrUpdate(gekoppeldeExtraTest);
			}
			laatsteScreeningRonde.setLaatsteIFOBTTest(buisMetUitslag);
			laatsteScreeningRonde.setLaatsteIFOBTTestExtra(gekoppeldeExtraTest);
			IFOBTTest nieuweLaatsteBuisGeslotenRonde = null;
			for (IFOBTTest test : screeningRonde.getIfobtTesten().stream().filter(i -> i.getType().equals(IFOBTType.GOLD)).collect(Collectors.toList()))
			{
				if (nieuweLaatsteBuisGeslotenRonde == null
					|| DateUtil.compareBefore(nieuweLaatsteBuisGeslotenRonde.getColonUitnodiging().getCreatieDatum(), test.getColonUitnodiging().getCreatieDatum()))
				{
					nieuweLaatsteBuisGeslotenRonde = test;
				}
			}
			screeningRonde.setLaatsteIFOBTTest(nieuweLaatsteBuisGeslotenRonde);
			screeningRonde.setLaatsteIFOBTTestExtra(null);
			hibernateService.saveOrUpdate(screeningRonde);
			screeningRonde = laatsteScreeningRonde;

		}
		return screeningRonde;
	}

	private void saveOrUpdateBuis(ColonUitnodiging uitnodiging)
	{
		IFOBTTest fobGold = uitnodiging.getGekoppeldeTest();
		if (fobGold != null)
		{
			hibernateService.saveOrUpdate(fobGold);
		}
		IFOBTTest studietest = uitnodiging.getGekoppeldeExtraTest();
		if (studietest != null)
		{
			hibernateService.saveOrUpdate(studietest);
		}
	}

	private void uitslagVerwijderen(IFOBTTest buis)
	{
		if (buis != null)
		{
			buis.setGeinterpreteerdeUitslag(null);
			buis.setNormWaarde(null);
			buis.setUitslag(null);
		}
	}

	@Override
	public void setStatus(IFOBTTest buis, IFOBTTestStatus nieuweStatus)
	{
		Date newStatusDatumTijd = currentDateSupplier.getDate();
		setStatusEnDatum(buis, nieuweStatus, newStatusDatumTijd);
	}

	private void setStatus(ColonUitnodiging uitnodiging, IFOBTTestStatus nieuweStatus)
	{
		Date newStatusDatumTijd = currentDateSupplier.getDate();
		setStatus(uitnodiging, nieuweStatus, newStatusDatumTijd);
	}

	private void setStatus(ColonUitnodiging uitnodiging, IFOBTTestStatus nieuweStatus, Date newStatusDatumTijd)
	{
		setStatusEnDatum(uitnodiging.getGekoppeldeTest(), nieuweStatus, newStatusDatumTijd);
		if (nieuweStatus.equals(IFOBTTestStatus.VERLOREN) || nieuweStatus.equals(IFOBTTestStatus.VERWIJDERD))
		{
			IFOBTTest extra = uitnodiging.getGekoppeldeExtraTest();
			if (extra != null)
			{
				setStatusEnDatum(extra, nieuweStatus, newStatusDatumTijd);
			}
		}
	}

	private void setStatusEnDatum(IFOBTTest buis, IFOBTTestStatus nieuweStatus, Date newStatusDatumTijd)
	{
		if (buis != null && (IFOBTTestStatus.ACTIEF.equals(nieuweStatus) && buis.getStatus() == null ||
			IFOBTTestStatus.VERLOREN.equals(nieuweStatus) && buis.getStatus() == null
			|| buis.getStatus().magWijzigenNaarStatus(nieuweStatus, buis)))
		{
			logStatusChange(buis, nieuweStatus);

			buis.setStatus(nieuweStatus);
			buis.setStatusDatum(newStatusDatumTijd);
		}
	}

	private static void logStatusChange(IFOBTTest buis, IFOBTTestStatus nieuweStatus)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("Status " + buis.getBarcode() + " " + buis.getStatus() + "->" + nieuweStatus);
		}
	}

	@Override
	public void checkVervaldatumVerlopen(IFOBTTest buis)
	{
		if (buis != null && IFOBTTestUtil.isGunstig(buis) && buis.getStatus() != IFOBTTestStatus.VERVALDATUMVERLOPEN)
		{
			if (!houdbaarheidService.isHoudbaar(IFOBTVervaldatum.class, buis.getBarcode()))
			{
				Date nu = currentDateSupplier.getDate();
				setStatusEnDatum(buis, IFOBTTestStatus.VERVALDATUMVERLOPEN, nu);
			}
		}
	}

	@Override
	public void heraanmelden(ColonScreeningRonde screeningRonde, LocalDateTime nu)
	{

		ColonDossier dossier = screeningRonde.getDossier();
		ColonAfmelding afmelding = AfmeldingUtil.getLaatsteAfmelding(screeningRonde, dossier);
		if (afmelding != null)
		{
			afmelding.setHeraanmeldingAfspraakBriefTegenhouden(false);
			afmelding.setHeraanmeldingBevestigingsBriefTegenhouden(true);
			afmelding.setHeraanmeldingAfspraakUitRooster(true);
			afmelding.setClientWilNieuweUitnodiging(false);
			baseAfmeldService.heraanmelden(afmelding, null);
		}
		if (ScreeningRondeStatus.AFGEROND.equals(screeningRonde.getStatus()))
		{
			screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
			screeningRonde.setStatusDatum(DateUtil.toUtilDate(nu));
			screeningRonde.setAfgerondReden(null);
			hibernateService.saveOrUpdate(screeningRonde);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging)
	{
		if (LOG.isTraceEnabled())
		{
			String logMessage = uitnodiging.getUitnodigingsId() + "(";
			if (uitnodiging.getGekoppeldeTest() != null)
			{
				logMessage += uitnodiging.getGekoppeldeTest().getBarcode();
			}
			if (uitnodiging.getGekoppeldeExtraTest() != null)
			{
				logMessage += "/" + uitnodiging.getGekoppeldeExtraTest().getBarcode();
			}
			LOG.trace("verwijderScannedAntwoordFormulier " + logMessage + ")");
		}
		ScannedAntwoordFormulier antwoordFormulier = uitnodiging.getAntwoordFormulier();
		IFOBTTest buis = IFOBTTestUtil.getIfobtTest(uitnodiging);

		hibernateService.saveOrUpdate(buis);

		antwoordFormulier.setStatus(ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER);
		hibernateService.saveOrUpdate(antwoordFormulier);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderUitslag(IFOBTTest buis, UploadDocument uploadDocument)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("verwijderUitslag " + buis.getBarcode());
		}
		ColonUitnodiging uitnodiging = IFOBTTestUtil.getUitnodiging(buis);
		ColonScreeningRonde ronde = buis.getColonScreeningRonde();
		IFOBTTest fobGold = uitnodiging.getGekoppeldeTest();
		IFOBTTest extra = uitnodiging.getGekoppeldeExtraTest();
		Date datum = currentDateSupplier.getDate();
		if (fobGold != null && fobGold.getUitslag() != null)
		{
			uitslagVerwijderen(fobGold);
			setStatusEnDatum(fobGold, IFOBTTestStatus.VERWIJDERD, datum);
		}
		else
		{
			setStatusEnDatum(fobGold, IFOBTTestStatus.ACTIEF, datum);
		}
		fobGold.setVerwijderbrief(uploadDocument);

		if (extra != null)
		{
			if (extra.getUitslag() != null)
			{
				setStatusEnDatum(extra, IFOBTTestStatus.VERWIJDERD, datum);
			}
			else if (extra.getType().equals(IFOBTType.STUDIE))
			{
				uitslagVerwijderen(extra);
				setStatusEnDatum(extra, IFOBTTestStatus.VERWIJDERD, datum);
			}
			else
			{
				setStatusEnDatum(extra, IFOBTTestStatus.ACTIEF, datum);
			}
			extra.setVerwijderbrief(uploadDocument);
		}
		saveOrUpdateBuis(uitnodiging);
		setVolgendeUitnodigingdatumAlsGunstig(ronde);
	}

	private void setVolgendeUitnodigingdatumAlsGunstig(ColonScreeningRonde ronde)
	{
		if (ColonScreeningRondeUtil.zijnErOngunstigeIfobts(ronde))
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);
		}
		else if (ColonScreeningRondeUtil.getEersteGunstigeTest(ronde) != null)
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GUNSTIGE_UITSLAG);
		}
		else
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.UITNODIGING_ONTVANGEN);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void markeerBuisAlsVerloren(ColonUitnodiging uitnodiging)
	{
		if (LOG.isTraceEnabled())
		{
			String logMessage = uitnodiging.getUitnodigingsId() + "(";
			if (uitnodiging.getGekoppeldeTest() != null)
			{
				logMessage += uitnodiging.getGekoppeldeTest().getBarcode();
			}
			if (uitnodiging.getGekoppeldeExtraTest() != null)
			{
				logMessage += "/" + uitnodiging.getGekoppeldeExtraTest().getBarcode();
			}
			LOG.trace("nieuweBuisAanvragen " + logMessage + ")");
		}
		setStatus(uitnodiging, IFOBTTestStatus.VERLOREN);
		studietestService.projectClientInactiverenBijVergelijkendOnderzoek(uitnodiging.getScreeningRonde());
		saveOrUpdateBuis(uitnodiging);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void monsterNietBeoordeelbaar(IFOBTTest ifobtTest)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("monsterNietBeoordeelbaar " + ifobtTest.getBarcode());
		}
		ColonScreeningRonde screeningRonde = uitslagNaarJuisteRonde(ifobtTest);
		bepaalEnSetHeraanmeldenTekstKey(ifobtTest);
		if (ifobtTest.getHeraanmeldenTekstKey() != null)
		{
			heraanmelden(screeningRonde, currentDateSupplier.getLocalDateTime());
		}
		setStatusEnDatum(ifobtTest, IFOBTTestStatus.NIETTEBEOORDELEN, currentDateSupplier.getDate());

		studietestService.projectClientInactiverenBijVergelijkendOnderzoek(screeningRonde);

		boolean briefGemaakt = maakBuitenDoelgroepBriefIndienNodig(screeningRonde, ifobtTest);
		if (!briefGemaakt)
		{
			ColonUitnodiging nieuweUitnodiging = screeningsrondeService.createNieuweUitnodiging(screeningRonde, ColonUitnodigingCategorie.U3);
			alsGeenPakketGemaaktDanExtraUitslagBriefMaken(ifobtTest, screeningRonde, nieuweUitnodiging);
		}
	}

	@Override
	public void bepaalEnSetHeraanmeldenTekstKey(IFOBTTest ifobtTest)
	{
		ColonScreeningRonde screeningRonde = ifobtTest.getColonScreeningRonde();
		ColonDossier dossier = screeningRonde.getDossier();
		ColonAfmelding colonAfmelding = AfmeldingUtil.getLaatsteAfmelding(screeningRonde, dossier);
		PreferenceKey heraanmeldenTekstKey = null;

		if (AfmeldingUtil.isActieveDefinitieveAfmelding(colonAfmelding))
		{
			heraanmeldenTekstKey = PreferenceKey.COLON_DEFINITIEF_HERAANMELDEN_TEKST;
		}
		else if (AfmeldingUtil.isActieveEenmaligeAfmelding(colonAfmelding))
		{
			heraanmeldenTekstKey = PreferenceKey.COLON_EENMALIG_HERAANMELDEN_TEKST;
		}
		ifobtTest.setHeraanmeldenTekstKey(heraanmeldenTekstKey);
	}

	@Override
	public void setTestenVerlorenIndienActief(IFOBTTest test)
	{
		if (test != null && !IFOBTTestStatus.isUnmutableEindStatus(test.getStatus()) && !IFOBTTestStatus.isMutableEindStatus(test.getStatus()))
		{
			ColonUitnodiging uitnodiging = IFOBTTestUtil.getUitnodiging(test);
			setStatus(uitnodiging, IFOBTTestStatus.VERLOREN);
			saveOrUpdateBuis(uitnodiging);
		}
	}

	private BigDecimal getIFOBTNormWaarde(ProjectClient projectClient)
	{
		if (projectClient != null && !projectClient.getActief()
			|| ProjectUtil.hasParameterSet(projectClient, ProjectParameterKey.COLON_FIT_NORM_WAARDE))
		{
			return new BigDecimal(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_FIT_NORM_WAARDE));
		}
		else
		{
			return BigDecimal.valueOf(simplePreferenceService.getInteger(PreferenceKey.IFOBT_NORM_WAARDE.name())).divide(BigDecimal.valueOf(100));
		}
	}

	public void setCurrentDateSupplier(ICurrentDateSupplier currentDateSupplier)
	{
		this.currentDateSupplier = currentDateSupplier;
	}
}
