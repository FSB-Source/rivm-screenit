package nl.rivm.screenit.service.colon.impl;

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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
import nl.rivm.screenit.dao.colon.ColonTestDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonMergedBrieven;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.service.impl.ImportBvoViaCsv;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.Location;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;

import org.apache.commons.lang.StringUtils;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ColonTestServiceImpl implements ColonTestService
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonTestServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private AfspraakDefinitieDao afspraakDefinitieDao;

	@Autowired
	private ColonHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private TestService testService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private ColonTestDao colonTestDao;

	@Autowired
	private RecurrenceService recurrenceService;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private ColonDossierBaseService dossierService;

	@Autowired
	private BaseDossierService baseDossierService;

	@Override
	public ColonConclusie maakAfspraakEnConclusie(GbaPersoon filter, Date fitVerwerkingsDatum)
	{
		var intakeAfspraak = maakAfspraak(filter, fitVerwerkingsDatum);
		var conclusie = intakeAfspraak.getConclusie();
		if (conclusie == null)
		{
			conclusie = new ColonConclusie();
			conclusie.setType(ColonConclusieType.COLOSCOPIE);
			conclusie.setDatum(currentDateSupplier.getDate());
			conclusie.setDatumColoscopie(DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(10)));
			var all = hibernateService.loadAll(InstellingGebruiker.class);
			conclusie.setInstellingGebruiker(all.get(0));
			intakeAfspraak.setConclusie(conclusie);
		}
		intakeAfspraak.setStatus(AfspraakStatus.UITGEVOERD);
		hibernateService.saveOrUpdate(intakeAfspraak);
		hibernateService.saveOrUpdate(conclusie);
		return conclusie;
	}

	@Override
	public ColonIntakeAfspraak maakAfspraak(GbaPersoon filter, boolean eenmalig, Date fitVerwerkingsDatum)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		var screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		var b2 = maakBrief(client, screeningRonde, BriefType.COLON_UITNODIGING_INTAKE);

		var uitnodiging = geefUitnodiging(screeningRonde);
		IFOBTTest fit = null;
		if (uitnodiging.getGekoppeldeTest() == null)
		{
			fit = maakHuidigeIFobtOntvangenEnOngunstig(filter);
		}
		else if (!FITTestUtil.isOngunstig(uitnodiging.getGekoppeldeTest()))
		{
			uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.STANDAARD);
			fit = maakHuidigeIFobtOntvangenEnOngunstig(filter);
		}
		if (fit != null && fitVerwerkingsDatum != null)
		{
			fit.setVerwerkingsDatum(fitVerwerkingsDatum);
		}
		uitnodiging.setCreatieDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(20)));
		uitnodiging.setVerstuurdDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(20)));
		hibernateService.saveOrUpdate(uitnodiging);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);
		hibernateService.saveOrUpdate(b2);

		var intakeAfspraak = screeningRonde.getLaatsteAfspraak();
		if (intakeAfspraak == null || !eenmalig)
		{
			intakeAfspraak = new ColonIntakeAfspraak();
			intakeAfspraak.setStatus(AfspraakStatus.GEPLAND);
			intakeAfspraak.setAfstand(BigDecimal.valueOf(2.3));
			intakeAfspraak.setAfspraaknummer(System.currentTimeMillis());
			intakeAfspraak.setClient(client);
			intakeAfspraak.setBezwaar(Boolean.FALSE);
			intakeAfspraak.setDatumLaatsteWijziging(currentDateSupplier.getDate());
			client.getAfspraken().add(intakeAfspraak);
			screeningRonde.setLaatsteAfspraak(intakeAfspraak);
			screeningRonde.getAfspraken().add(intakeAfspraak);
			intakeAfspraak.setColonScreeningRonde(screeningRonde);
			intakeAfspraak.getDisciplines().add(hibernateService.loadAll(Discipline.class).get(0));
		}
		intakeAfspraak.setBezwaar(Boolean.FALSE);
		intakeAfspraak.setStatus(AfspraakStatus.GEPLAND);
		if (intakeAfspraak.getStartTime() == null)
		{
			intakeAfspraak.setStartTime(DateUtil.plusDagen(currentDateSupplier.getDate(), 1));
		}
		if (intakeAfspraak.getLocation() == null)
		{
			var all = hibernateService.loadAll(Kamer.class);
			all.sort(Comparator.comparing(Location::getId));
			intakeAfspraak.setLocation(all.get(0));
		}
		if (intakeAfspraak.getDefinition() == null)
		{
			var afspraakDefinities = afspraakDefinitieDao.getActieveActieDefinities(intakeAfspraak.getLocation().getColoscopieCentrum());
			intakeAfspraak.setDefinition(afspraakDefinities.get(0));
		}
		if (intakeAfspraak.getEndTime() == null)
		{
			intakeAfspraak.setEndTime(DateUtil.plusTijdseenheid(intakeAfspraak.getStartTime(), intakeAfspraak.getDefinition().getDuurAfspraakInMinuten(), ChronoUnit.MINUTES));
		}
		b2.setIntakeAfspraak(intakeAfspraak);
		hibernateService.saveOrUpdate(b2);
		hibernateService.saveOrUpdate(intakeAfspraak);
		hibernateService.saveOrUpdate(screeningRonde);
		hibernateService.saveOrUpdate(client);
		dossierService.setDatumVolgendeUitnodiging(screeningRonde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);
		return intakeAfspraak;
	}

	@Override
	public ColonBrief maakBrief(Client client, ColonScreeningRonde screeningRonde, BriefType briefType)
	{
		var brief = new ColonBrief();
		brief.setTemplateNaam("testBrief.doc");
		brief.setBriefType(briefType);
		brief.setClient(client);
		brief.setCreatieDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(3)));
		brief.setScreeningRonde(screeningRonde);
		screeningRonde.getBrieven().add(brief);
		hibernateService.saveOrUpdateAll(client, screeningRonde, brief);
		return brief;
	}

	@Override
	public ColonIntakeAfspraak maakAfspraak(GbaPersoon filter, Date fitVerwerkingsDatum)
	{
		return maakAfspraak(filter, true, fitVerwerkingsDatum);
	}

	private void maakVooraankonding(Client client, ColonDossier dossier)
	{
		if (dossier.getColonVooraankondiging() == null)
		{
			var b = new ColonBrief();
			b.setTemplateNaam("vooraankondigingsbrief.doc");
			b.setBriefType(BriefType.COLON_VOORAANKONDIGING);
			b.setClient(client);
			b.setCreatieDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(27)));

			var voor = new ColonVooraankondiging();
			voor.setCreatieDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(27)));
			voor.setBrief(b);
			dossier.setColonVooraankondiging(voor);
			hibernateService.saveOrUpdate(voor);
			hibernateService.saveOrUpdate(b);
		}
	}

	@Override
	public IFOBTTest zetVelorenIfobt(GbaPersoon filter, boolean zetUitslag, boolean gunstig)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		var screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		var uitnodigingen = screeningRonde.getUitnodigingen();
		ColonUitnodiging uitnodiging = null;
		for (var uit : uitnodigingen)
		{
			var ifobt = uit.getGekoppeldeTest();
			if (IFOBTTestStatus.VERLOREN.equals(ifobt.getStatus()))
			{
				uitnodiging = uit;
				break;
			}
		}
		var ifobt = uitnodiging.getGekoppeldeTest();
		if (zetUitslag)
		{
			ifobt.setNormWaarde(new BigDecimal(20));
			if (gunstig)
			{
				ifobt.setUitslag(BigDecimal.TEN);
			}
			else
			{
				ifobt.setUitslag(new BigDecimal(30));
			}
			ifobt.setStatus(IFOBTTestStatus.UITGEVOERD);
		}
		hibernateService.saveOrUpdate(ifobt);

		return ifobt;
	}

	@Override
	public void huidigeIFOBTvoorRapelDatum(GbaPersoon filter)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		var screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		var test = screeningRonde.getLaatsteIFOBTTest();
		var date = currentDateSupplier.getDate();
		int rapelDagen = simplePreferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
		date = DateUtil.minDagen(date, rapelDagen + 2);
		var rapelDate = date;
		test.setStatusDatum(rapelDate);
		hibernateService.saveOrUpdate(test);

		var uitnodiging = screeningRonde.getLaatsteUitnodiging();
		uitnodiging.setVerstuurdDatum(DateUtil.plusTijdseenheid(date, 20, ChronoUnit.SECONDS));
		hibernateService.saveOrUpdate(uitnodiging);

		uitnodiging = geefUitnodiging(screeningRonde);
		geefIFobttest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(uitnodiging);
	}

	@Override
	public void maakClientKlaarVoorRappeleren(GbaPersoon filter)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		hibernateService.saveOrUpdate(client);
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		var screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		var uitnodiging = geefUitnodiging(screeningRonde);
		geefIFobttest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(uitnodiging);
	}

	@Override
	public void maakClientKlaarVoorAfronden(GbaPersoon filter)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		hibernateService.saveOrUpdate(client);
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		var screeningRonde = geefScreeningRonde(dossier);

		var date = currentDateSupplier.getDate();
		int uitnodigingsDagen = simplePreferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		date = DateUtil.minDagen(date, uitnodigingsDagen + 1);
		var uitnodigingsDate = date;

		if (screeningRonde.getCreatieDatum() == null)
		{
			screeningRonde.setCreatieDatum(uitnodigingsDate);
		}
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		var uitnodiging = geefUitnodiging(screeningRonde);
		var test = geefIFobttest(uitnodiging, screeningRonde);
		test.setNormWaarde(new BigDecimal("20.00"));
		test.setUitslag(new BigDecimal("10.00"));
		test.setColonScreeningRonde(screeningRonde);
		test.setStatusDatum(currentDateSupplier.getDate());
		test.setVerwerkingsDatum(currentDateSupplier.getDate());
		test.setAnalyseDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(test);
		uitnodiging.setVerstuurdDatum(uitnodigingsDate);
		uitnodiging.setTemplateNaam("2-Brief uitnodiging FIT.doc");
		uitnodiging.setVerstuurd(Boolean.TRUE);
		uitnodiging.setColonUitnodigingCategorie(ColonUitnodigingCategorie.U2);
		hibernateService.saveOrUpdate(uitnodiging);

	}

	@Override
	public IFOBTTest maakHuidigeIFobtOntvangenEnGunstig(GbaPersoon filter)
	{

		return maakHuidigeIFobtOntvangenInclUitslag(filter, new BigDecimal(20), BigDecimal.TEN);
	}

	@Override
	public IFOBTTest maakHuidigeIFobtOntvangenEnOngunstig(GbaPersoon filter)
	{

		return maakHuidigeIFobtOntvangenInclUitslag(filter, BigDecimal.TEN, new BigDecimal(20));
	}

	@Override
	public IFOBTTest maakHuidigeIFobtOntvangenInclUitslag(GbaPersoon filter, BigDecimal normwaarde, BigDecimal uitslag)
	{

		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		hibernateService.saveOrUpdate(client);
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		hibernateService.saveOrUpdate(dossier);
		var screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(screeningRonde);
		var uitnodiging = geefUitnodiging(screeningRonde);

		if (uitnodiging.getGekoppeldeTest() != null)
		{
			uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.STANDAARD);
		}

		var test = geefIFobttest(uitnodiging, screeningRonde);

		test.setNormWaarde(normwaarde);
		test.setUitslag(uitslag);

		if (test.getStatus() == IFOBTTestStatus.ACTIEF && !FITTestUtil.isOngunstig(test))
		{
			test.setStatus(IFOBTTestStatus.UITGEVOERD);
		}
		else if (test.getStatus() == IFOBTTestStatus.ACTIEF || FITTestUtil.isOngunstig(test))
		{
			test.setStatus(IFOBTTestStatus.UITGEVOERD);
		}
		test.setStatusDatum(currentDateSupplier.getDate());
		test.setVerwerkingsDatum(currentDateSupplier.getDate());
		test.setAnalyseDatum(currentDateSupplier.getDate());

		return test;
	}

	@Override
	public void maakUitnodigingEnTestenVergelijkendOnderzoek(GbaPersoon filter)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		hibernateService.saveOrUpdate(client);
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		hibernateService.saveOrUpdate(dossier);
		var screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		var uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.VERGELIJKEND);
		hibernateService.saveOrUpdate(uitnodiging);
		var test = geefIFobttest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(test);
		var studietest = geefStudietest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(studietest);
		hibernateService.saveOrUpdateAll(dossier, screeningRonde, client);
	}

	private ColonUitnodiging maakNieuweUitnodiging(ColonScreeningRonde screeningRonde, ColonOnderzoeksVariant onderzoeksVariant)
	{
		var uitnodiging = new ColonUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		uitnodiging.setCreatieDatum(currentDateSupplier.getDate());
		uitnodiging.setScreeningRonde(screeningRonde);
		uitnodiging.setUitnodigingsDatum(currentDateSupplier.getDate());
		uitnodiging.setVerstuurdDatum(currentDateSupplier.getDate());
		if (ColonOnderzoeksVariant.STANDAARD.equals(onderzoeksVariant))
		{
			uitnodiging.setTemplateNaam("2-Brief uitnodiging FIT.doc");
			uitnodiging.setColonUitnodigingCategorie(ColonUitnodigingCategorie.U2);
			uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.STANDAARD);
		}
		if (ColonOnderzoeksVariant.VERGELIJKEND.equals(onderzoeksVariant))
		{
			uitnodiging.setColonUitnodigingCategorie(ColonUitnodigingCategorie.U1);
			uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.VERGELIJKEND);
		}
		hibernateService.saveOrUpdate(uitnodiging);

		screeningRonde.setLaatsteUitnodiging(uitnodiging);
		hibernateService.saveOrUpdate(screeningRonde);

		return uitnodiging;
	}

	private ColonUitnodiging geefUitnodiging(ColonScreeningRonde screeningRonde)
	{
		var uitnodiging = screeningRonde.getLaatsteUitnodiging();
		if (uitnodiging == null)
		{
			uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.STANDAARD);
		}

		return uitnodiging;
	}

	private IFOBTTest geefIFobttest(ColonUitnodiging uitnodiging, ColonScreeningRonde screeningRonde)
	{
		var ifobt = uitnodiging.getGekoppeldeTest();
		if (ifobt == null)
		{
			ifobt = new IFOBTTest();
			var date = currentDateSupplier.getLocalDateTime();
			int rapelDagen = simplePreferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			date = date.minusDays(rapelDagen + 1);
			var rapelDate = DateUtil.toUtilDate(date);
			ifobt.setBarcode("TGD" + FITTestUtil.getFITTestBarcode(uitnodiging.getUitnodigingsId()));
			ifobt.setStatus(IFOBTTestStatus.ACTIEF);
			ifobt.setColonScreeningRonde(uitnodiging.getScreeningRonde());
			ifobt.setHerinnering(Boolean.FALSE);
			ifobt.setDatumVerstuurd(rapelDate);
			ifobt.setStatusDatum(rapelDate);
			ifobt.setColonUitnodiging(uitnodiging);
			ifobt.setType(IFOBTType.GOLD);
			hibernateService.saveOrUpdate(ifobt);
			uitnodiging.setGekoppeldeTest(ifobt);
			uitnodiging.setVerstuurdDatum(DateUtil.toUtilDate(date.minusSeconds(20)));
			uitnodiging.setVerstuurd(Boolean.TRUE);
			uitnodiging.setVerstuurdDoorInpakcentrum(true);
			hibernateService.saveOrUpdate(uitnodiging);
			screeningRonde.setLaatsteIFOBTTest(ifobt);
			screeningRonde.getIfobtTesten().add(ifobt);
			hibernateService.saveOrUpdate(screeningRonde);
			var colonVooraankondiging = screeningRonde.getDossier().getColonVooraankondiging();
			colonVooraankondiging.setCreatieDatum(DateUtil.toUtilDate(date.minusDays(14)));
			hibernateService.saveOrUpdate(colonVooraankondiging);
		}
		return ifobt;
	}

	private IFOBTTest geefStudietest(ColonUitnodiging uitnodiging, ColonScreeningRonde screeningRonde)
	{
		var studietest = new IFOBTTest();
		studietest.setBarcode("TST" + FITTestUtil.getFITTestBarcode(uitnodiging.getUitnodigingsId()));
		studietest.setStatus(IFOBTTestStatus.ACTIEF);
		studietest.setColonScreeningRonde(uitnodiging.getScreeningRonde());
		studietest.setHerinnering(Boolean.FALSE);
		studietest.setDatumVerstuurd(currentDateSupplier.getDate());
		studietest.setStatusDatum(currentDateSupplier.getDate());
		studietest.setColonUitnodigingExtra(uitnodiging);
		studietest.setType(IFOBTType.STUDIE);
		hibernateService.saveOrUpdate(studietest);
		uitnodiging.setGekoppeldeExtraTest(studietest);
		hibernateService.saveOrUpdate(uitnodiging);
		screeningRonde.setLaatsteIFOBTTestExtra(studietest);
		screeningRonde.getIfobtTesten().add(studietest);
		hibernateService.saveOrUpdate(screeningRonde);

		return studietest;
	}

	private ColonScreeningRonde geefScreeningRonde(ColonDossier dossier)
	{
		var screeningRonde = dossier.getLaatsteScreeningRonde();
		if (screeningRonde == null)
		{
			screeningRonde = maakNieuweScreeningRonde(dossier);
		}
		return screeningRonde;
	}

	@Override
	public ColonScreeningRonde maakNieuweScreeningRonde(ColonDossier dossier)
	{
		var screeningRonde = new ColonScreeningRonde();
		screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
		screeningRonde.setAangemeld(true);
		screeningRonde.setCreatieDatum(currentDateSupplier.getDate());
		dossier.setLaatsteScreeningRonde(screeningRonde);
		dossier.getScreeningRondes().add(screeningRonde);
		screeningRonde.setDossier(dossier);
		hibernateService.saveOrUpdateAll(screeningRonde, dossier);

		return screeningRonde;
	}

	@Override
	public void huisartsBerichtKlaarzetten(GbaPersoon filter, HuisartsBerichtType berichtType)
	{
		var client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		var dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		var ronde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(client);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(ronde);

		var context = new MailMergeContext();
		context.setClient(client);

		huisartsBerichtService.verstuurColonHuisartsBericht(client, ronde, berichtType, context);
	}

	private Client geefClient(String bsn, Date geboortedatum, Date overlijdensDatum)
	{
		var client = clientDao.getClientByBsn(bsn);
		if (client == null)
		{
			client = new Client();
			var persoon = new GbaPersoon();
			client.setPersoon(persoon);
			persoon.setVoornaam("John");
			persoon.setGbaGeboorteLand(testService.getGbaLand());
			persoon.setAchternaam("Doe-" + bsn);
			persoon.setGeslacht(Geslacht.MAN);
			persoon.setBsn(bsn);
			if (geboortedatum == null)
			{
				try
				{
					persoon.setGeboortedatum(new SimpleDateFormat("dd-MM-yyyy").parse("01-01-1950"));
				}
				catch (ParseException e)
				{
					LOG.error("Er is een fout opgetreden! " + e.getMessage(), e);
				}
			}
			else
			{
				persoon.setGeboortedatum(geboortedatum);
			}
			persoon.setPatient(client);
			client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
			hibernateService.saveOrUpdate(persoon);
			hibernateService.saveOrUpdate(client);

			dossierFactory.maakDossiers(client);
		}
		else if (client != null)
		{
			var persoon = client.getPersoon();
			if (geboortedatum != null)
			{
				persoon.setGeboortedatum(geboortedatum);
			}
			persoon.setOverlijdensdatum(overlijdensDatum);
			hibernateService.saveOrUpdate(persoon);
		}
		return client;
	}

	private BagAdres geefAdres(Client client, Gemeente gemeente)
	{
		var gbaAdres = client.getPersoon().getGbaAdres();
		if (gbaAdres == null)
		{
			gbaAdres = new BagAdres();
			hibernateService.saveOrUpdate(client.getPersoon());
			client.getPersoon().setGbaAdres(gbaAdres);
			hibernateService.saveOrUpdate(gbaAdres);
			if (gemeente == null)
			{
				gemeente = (Gemeente) hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.isNotNull("screeningOrganisatie"))
					.addOrder(Order.asc("naam")).list().get(0);
			}
			gbaAdres.setGbaGemeente(gemeente);
			gbaAdres.setPlaats(gemeente.getNaam());
			gbaAdres.setStraat("Teststraat");
			gbaAdres.setHuisnummer(9);
			var postcode = "1111XX";
			gbaAdres.setPostcode(postcode);
			hibernateService.saveOrUpdate(gbaAdres);
		}
		else if (gemeente != null)
		{
			gbaAdres.setGbaGemeente(gemeente);
			gbaAdres.setPlaats(gemeente.getNaam());
			hibernateService.saveOrUpdate(gbaAdres);
		}
		return gbaAdres;
	}

	@Override
	public void brievenKlaarzetten(int aantal)
	{
		for (var i = 0; i < aantal; i++)
		{
			var persoon = new GbaPersoon();
			persoon.setGeslacht(Geslacht.MAN);
			persoon.setGbaAdres(new BagAdres());
			persoon.setBsn(TestBsnGenerator.getValideBsn());
			maakClientKlaarVoorRappeleren(persoon);
			hibernateService.getHibernateSession().flush();
			var client = geefClient(persoon.getBsn(), null, null);
			var ronde = geefScreeningRonde(client.getColonDossier());
			briefService.maakBvoBrief(ronde, BriefType.COLON_HERINNERING, currentDateSupplier.getDate());
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void importColonClientenViaCsv(File file, Map<String, ColonUitnodigingCategorie> categoriePerPatient, int startRondeCorrectie) throws IOException, ParseException
	{
		testService.importClientenViaCsv(file, new ImportBvoViaCsv()
		{
			Integer trackTraceId = Integer.valueOf(0);

			@Override
			protected void importBvoViaCsv(Map<String, Integer> headersIndex, String[] columns, Client client)
			{

				var aantalDagenVooraankondigingCell = columns[headersIndex.get("DatumVooraankondiging")];
				if (StringUtils.isNotBlank(aantalDagenVooraankondigingCell) && !aantalDagenVooraankondigingCell.equals("0"))
				{
					var colonDossier = new ColonDossier();
					colonDossier.setStatus(DossierStatus.ACTIEF);
					colonDossier.setAangemeld(true);
					colonDossier.setClient(client);
					client.setColonDossier(colonDossier);

					var aantalDagenVooraankondiging = Integer.parseInt(aantalDagenVooraankondigingCell) + startRondeCorrectie;
					var datumVooraankondiging = DateUtil.plusDagen(currentDateSupplier.getDate(), aantalDagenVooraankondiging);

					var statusRonde = columns[headersIndex.get("StatusRonde")];
					if (StringUtils.isNotBlank(statusRonde))
					{
						var colonVooraankondiging = new ColonVooraankondiging();
						var brief = new ColonBrief();
						colonVooraankondiging.setBrief(brief);
						colonVooraankondiging.setClient(client);
						colonVooraankondiging.setCreatieDatum(datumVooraankondiging);

						brief.setBriefType(BriefType.COLON_VOORAANKONDIGING);
						brief.setCreatieDatum(datumVooraankondiging);
						brief.setGegenereerd(true);
						brief.setTemplateNaam("vooraankondiging.doc");
						brief.setClient(client);

						var mergedBrieven = new ColonMergedBrieven();
						mergedBrieven.setBriefType(BriefType.COLON_VOORAANKONDIGING);
						mergedBrieven.setCreatieDatum(datumVooraankondiging);
						mergedBrieven.setGeprint(true);
						mergedBrieven.setPrintDatum(datumVooraankondiging);
						mergedBrieven.setVerwijderd(true);
						brief.setMergedBrieven(mergedBrieven);
						mergedBrieven.getBrieven().add(brief);
						colonDossier.setColonVooraankondiging(colonVooraankondiging);

						ColonUitnodigingCategorie colonUitnodigingCategorie = null;
						var cat = columns[headersIndex.get(" CAT ")];
						if (StringUtils.isNumeric(cat))
						{
							switch (Integer.parseInt(cat.trim()))
							{
							case 1:
								colonUitnodigingCategorie = ColonUitnodigingCategorie.U1;
								break;
							case 2:
								colonUitnodigingCategorie = ColonUitnodigingCategorie.U2;
								break;
							case 3:
								colonUitnodigingCategorie = ColonUitnodigingCategorie.U3;
								break;
							case 4:
								colonUitnodigingCategorie = ColonUitnodigingCategorie.U4;
								break;
							case 6:
								colonUitnodigingCategorie = ColonUitnodigingCategorie.U6;
								break;
							default:
								break;
							}
						}

						categoriePerPatient.put(client.getPersoon().getBsn(), colonUitnodigingCategorie);
						var aantalDagendatumRondeDatum = Integer.parseInt(columns[headersIndex.get("DatumRonde")]) + startRondeCorrectie;
						var datumRondeDatum = DateUtil.plusDagen(currentDateSupplier.getDate(), aantalDagendatumRondeDatum);

						var colonScreeningRonde = new ColonScreeningRonde();
						colonScreeningRonde.setCreatieDatum(datumRondeDatum);
						colonScreeningRonde.setStatus(ScreeningRondeStatus.valueOf(statusRonde));
						colonScreeningRonde.setStatusDatum(datumRondeDatum);
						colonScreeningRonde.setDossier(colonDossier);
						colonScreeningRonde.setAangemeld(true);
						colonDossier.getScreeningRondes().add(colonScreeningRonde);
						colonDossier.setLaatsteScreeningRonde(colonScreeningRonde);
						brief.setScreeningRonde(colonScreeningRonde);

						var uitnodiging = new ColonUitnodiging();
						uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
						uitnodiging.setScreeningRonde(colonScreeningRonde);
						uitnodiging.setColonUitnodigingCategorie(colonUitnodigingCategorie);
						uitnodiging.setCreatieDatum(datumRondeDatum);
						uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.STANDAARD);
						var ifobtOntvangen = columns[headersIndex.get("IFOBT ontvangen")];
						if (StringUtils.isNotBlank(columns[headersIndex.get("Uitnodiging")]))
						{
							uitnodiging.setUitnodigingsDatum(datumRondeDatum);
							uitnodiging.setVerstuurd(true);
							uitnodiging.setVerstuurdDatum(DateUtil.minDagen(datumRondeDatum, 2));
						}
						else
						{
							uitnodiging.setUitnodigingsDatum(DateUtil.plusDagen(datumRondeDatum, 10));
							uitnodiging.setVerstuurd(false);
						}
						colonScreeningRonde.setLaatsteUitnodiging(uitnodiging);
						colonScreeningRonde.getUitnodigingen().add(uitnodiging);
						colonScreeningRonde.getBrieven().add(brief);
						colonScreeningRonde.setLaatsteBrief(brief);

						if (StringUtils.isNotBlank(columns[headersIndex.get("Uitnodiging")]))
						{
							var ifobtTest = new IFOBTTest();
							if (colonUitnodigingCategorie == ColonUitnodigingCategorie.U3)
							{
								ifobtTest.setRedenNietTeBeoordelen(RedenNietTeBeoordelen.BARCODE_ONLEESBAAR);
							}
							colonScreeningRonde.getIfobtTesten().add(ifobtTest);
							colonScreeningRonde.setLaatsteIFOBTTest(ifobtTest);
							ifobtTest.setColonScreeningRonde(colonScreeningRonde);
							ifobtTest.setStatusDatum(DateUtil.minusTijdseenheid(datumRondeDatum, 1, ChronoUnit.SECONDS));
							ifobtTest.setBarcode("AAB" + StringUtils.leftPad("" + trackTraceId++, 4, "0"));
							ifobtTest.setDatumVerstuurd(DateUtil.minDagen(datumRondeDatum, 12));
							ifobtTest.setType(IFOBTType.GOLD);
							uitnodiging.setVerstuurdDoorInpakcentrum(true);
							uitnodiging.setGekoppeldeTest(ifobtTest);

							uitnodiging.setTrackTraceId("" + trackTraceId);
							ifobtTest.setColonUitnodiging(uitnodiging);

							if (StringUtils.isNotBlank(ifobtOntvangen))
							{
								ifobtTest.setVerwerkingsDatum(DateUtil.minDagen(datumRondeDatum, 12));
								ifobtTest.setAnalyseDatum(DateUtil.minDagen(datumRondeDatum, 11));
								ifobtTest.setUitslag(BigDecimal.valueOf(Integer.parseInt(columns[headersIndex.get("IFOBT uitslag")])));
								ifobtTest.setNormWaarde(BigDecimal.valueOf(Integer.parseInt(columns[headersIndex.get("IFOBT norm")])));
								var statusTest = columns[headersIndex.get("StatusTest")];
								if (StringUtils.isNotBlank(statusTest))
								{
									var testStatus = IFOBTTestStatus.valueOf(statusTest);
									ifobtTest.setStatus(testStatus);
								}
								else
								{
									ifobtTest.setStatus(IFOBTTestStatus.UITGEVOERD);
								}
							}
							else
							{
								ifobtTest.setStatus(IFOBTTestStatus.ACTIEF);
							}
						}
					}

				}
			}
		});
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public String clientenResetten(String bsns)
	{
		var gevondenEnIsVerwijderd = 0;
		var bsnList = bsns.split(",");
		var result = "Succesvol";
		for (var bsn : bsnList)
		{
			try
			{
				if (StringUtils.isBlank(bsn) || bsn.trim().length() != 9)
				{
					continue;
				}
				var client = clientDao.getClientByBsn(bsn.trim());
				if (client == null)
				{
					continue;
				}
				clientReset(client);
				testService.verwijderClientContacten(client, Bevolkingsonderzoek.COLON);
				gevondenEnIsVerwijderd++;
			}
			catch (Exception e)
			{
				result = "Fout bij resetten van client met BSN " + bsn;
				LOG.error("error bij bsn " + bsn, e);
			}
		}
		return result + ". #" + gevondenEnIsVerwijderd + " clienten gereset.";
	}

	@Override
	public void clientReset(Client client)
	{
		var dossier = client.getColonDossier();
		if (dossier == null)
		{
			return;
		}
		dossierService.maakDossierLeeg(dossier);

		baseDossierService.verwijderLaatsteAfmelding(dossier);

		var overgeblevenBrieven = hibernateService.getByParameters(ColonBrief.class, Map.of("client", client));
		hibernateService.deleteAll(overgeblevenBrieven);

		client.setColonDossier(null);
		hibernateService.saveOrUpdate(client);
		hibernateService.delete(dossier);

		dossierFactory.maakDossiers(client);
	}

	@Override
	public int markeerNogNietNaarInpakcentrumVerstuurdeUitnodigingenAlsVerstuurd()
	{
		return colonTestDao.markeerNogNietNaarInpakcentrumVerstuurdeUitnodigingenAlsVerstuurd();
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public int verwijderRoosterBlokken()
	{

		var aantalRoosterItemsVerwijderd = 0;

		List<Long> alleRoosterBlokken = hibernateService.getHibernateSession().createCriteria(RoosterItem.class).add(Restrictions.isNotEmpty("afspraken"))
			.setProjection(Projections.id()).list();

		for (var roosterBlokId : alleRoosterBlokken)
		{
			var roosterBlok = hibernateService.get(RoosterItem.class, roosterBlokId);
			if (!roosterBlok.getAfspraken().isEmpty())
			{
				for (var afspraak : roosterBlok.getAfspraken())
				{
					afspraak.setRoosterItem(null);
					hibernateService.saveOrUpdate(afspraak);
				}
				roosterBlok.getAfspraken().clear();
				hibernateService.saveOrUpdate(roosterBlok);
			}
		}
		List<Long> alleRecurrences = hibernateService.getHibernateSession().createCriteria(AbstractRecurrence.class).setProjection(Projections.id()).list();
		for (var recurrenceId : alleRecurrences)
		{
			var recurrence = hibernateService.get(AbstractRecurrence.class, recurrenceId);
			if (recurrence != null && !NoRecurrence.class.isAssignableFrom(recurrence.getClass()))
			{
				aantalRoosterItemsVerwijderd += recurrence.getAppointments().size();
				recurrenceService.deleteHerhalingChain(recurrence);
			}
		}

		alleRoosterBlokken = hibernateService.getHibernateSession().createCriteria(RoosterItem.class).add(Restrictions.isNull("recurrence")).setProjection(Projections.id()).list();
		for (var roosterBlokId : alleRoosterBlokken)
		{
			var roosterBlok = hibernateService.get(RoosterItem.class, roosterBlokId);
			if (roosterBlok != null)
			{
				aantalRoosterItemsVerwijderd++;
				hibernateService.delete(roosterBlok);
			}
		}

		return aantalRoosterItemsVerwijderd;
	}

	@Override
	public ColonUitnodigingCategorie getUitnodigingCategorie(Client client)
	{
		if (client.getPersoon().getOverlijdensdatum() == null && client.getColonDossier().getColonVooraankondiging() != null
			&& client.getPersoon().getDatumVertrokkenUitNederland() == null)
		{
			var datumVooraankondiging = client.getColonDossier().getColonVooraankondiging().getCreatieDatum();
			var datumVooraankondigingPlusPeriode = DateUtil.plusDagen(datumVooraankondiging,
				simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name()));
			if (datumVooraankondigingPlusPeriode.before(currentDateSupplier.getDate()) && client.getColonDossier().getScreeningRondes().size() == 0)
			{
				return ColonUitnodigingCategorie.U1;
			}

			if (client.getColonDossier().getLaatsteScreeningRonde() != null)
			{
				var datumVolgendeUitnodiging = dossierService.getDatumVolgendeUitnodiging(client.getColonDossier());
				var volgendeUitnodigingsDatumBereikt = !datumVolgendeUitnodiging.isAfter(currentDateSupplier.getLocalDate());
				if (client.getColonDossier().getLaatsteScreeningRonde().getStatus() == ScreeningRondeStatus.AFGEROND
					&& volgendeUitnodigingsDatumBereikt)
				{
					return ColonUitnodigingCategorie.U2;
				}

				if (client.getColonDossier().getLaatsteScreeningRonde().getStatus() == ScreeningRondeStatus.LOPEND
					&& client.getColonDossier().getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null)
				{
					ColonUitnodigingCategorie colonUitnodigingCategorie = null;
					switch (client.getColonDossier().getLaatsteScreeningRonde().getLaatsteIFOBTTest().getStatus())
					{
					case NIETTEBEOORDELEN:
						colonUitnodigingCategorie = ColonUitnodigingCategorie.U3;
						break;
					case VERLOREN:
						colonUitnodigingCategorie = ColonUitnodigingCategorie.U4;
						break;
					case ONBETROUWBAAR:
						colonUitnodigingCategorie = ColonUitnodigingCategorie.U5;
						break;
					case VERVALDATUMVERLOPEN:
						colonUitnodigingCategorie = ColonUitnodigingCategorie.U6;
						break;
					case WELBRIEFGEENTEST:
						colonUitnodigingCategorie = ColonUitnodigingCategorie.U7;
						break;
					case WELTESTGEENBRIEF:
						colonUitnodigingCategorie = ColonUitnodigingCategorie.U8;
						break;
					default:
						break;
					}

					return colonUitnodigingCategorie;
				}
			}
		}

		return null;
	}

}
