package nl.rivm.screenit.service.colon.impl;

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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
import nl.rivm.screenit.dao.colon.ColonTestDao;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonAfmelding;
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
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.service.impl.ImportBvoViaCsv;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;

import org.apache.commons.lang.StringUtils;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ImmutableMap;

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
	private ClientService clientService;

	@Autowired
	private RecurrenceService recurrenceService;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private ColonDossierBaseService dossierService;

	@Override
	public ColonConclusie maakAfspraakEnConclusie(GbaPersoon filter, Date fitVerwerkingsDatum)
	{
		ColonIntakeAfspraak intakeAfspraak = maakAfspraak(filter, fitVerwerkingsDatum);
		ColonConclusie conclusie = intakeAfspraak.getConclusie();
		if (conclusie == null)
		{
			conclusie = new ColonConclusie();
			conclusie.setType(ColonConclusieType.COLOSCOPIE);
			conclusie.setDatum(currentDateSupplier.getDate());
			conclusie.setDatumColoscopie(DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(10)));
			List<InstellingGebruiker> all = hibernateService.loadAll(InstellingGebruiker.class);
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
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		ColonBrief b2 = maakBrief(client, screeningRonde, BriefType.COLON_UITNODIGING_INTAKE);

		ColonUitnodiging uitnodiging = geefUitnodiging(screeningRonde);
		IFOBTTest fit = null;
		if (uitnodiging.getGekoppeldeTest() == null)
		{
			fit = maakHuidigeIFobtOntvangenEnOngunstig(filter);
		}
		else if (!IFOBTTestUtil.isOngunstig(uitnodiging.getGekoppeldeTest()))
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

		ColonIntakeAfspraak intakeAfspraak = screeningRonde.getLaatsteAfspraak();
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
			intakeAfspraak.setStartTime(currentDateSupplier.getDateTime().plusDays(1).toDate());
		}
		if (intakeAfspraak.getLocation() == null)
		{
			List<Kamer> all = hibernateService.loadAll(Kamer.class);
			Collections.sort(all, new Comparator<Kamer>()
			{
				@Override
				public int compare(Kamer o1, Kamer o2)
				{
					return o1.getId().compareTo(o2.getId());
				}
			});
			intakeAfspraak.setLocation(all.get(0));
		}
		if (intakeAfspraak.getDefinition() == null)
		{
			List<AfspraakDefinitie> afspraakDefinities = afspraakDefinitieDao.getActieveActieDefinities(intakeAfspraak.getLocation().getColoscopieCentrum());
			intakeAfspraak.setDefinition(afspraakDefinities.get(0));
		}
		if (intakeAfspraak.getEndTime() == null)
		{
			intakeAfspraak.setEndTime(new DateTime(intakeAfspraak.getStartTime()).plusMinutes(intakeAfspraak.getDefinition().getDuurAfspraakInMinuten()).toDate());
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
		ColonBrief brief = new ColonBrief();
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
			ColonBrief b = new ColonBrief();
			b.setTemplateNaam("vooraankondigingsbrief.doc");
			b.setBriefType(BriefType.COLON_VOORAANKONDIGING);
			b.setClient(client);
			b.setCreatieDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusDays(27)));

			ColonVooraankondiging voor = new ColonVooraankondiging();
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
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		List<ColonUitnodiging> uitnodigingen = screeningRonde.getUitnodigingen();
		ColonUitnodiging uitnodiging = null;
		for (ColonUitnodiging uit : uitnodigingen)
		{
			IFOBTTest ifobt = uit.getGekoppeldeTest();
			if (IFOBTTestStatus.VERLOREN.equals(ifobt.getStatus()))
			{
				uitnodiging = uit;
				break;
			}
		}
		IFOBTTest ifobt = uitnodiging.getGekoppeldeTest();
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
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		IFOBTTest test = screeningRonde.getLaatsteIFOBTTest();
		DateTime date = currentDateSupplier.getDateTime();
		int rapelDagen = simplePreferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
		date = date.minusDays(rapelDagen + 2);
		Date rapelDate = date.toDate();
		test.setStatusDatum(rapelDate);
		hibernateService.saveOrUpdate(test);

		ColonUitnodiging uitnodiging = screeningRonde.getLaatsteUitnodiging();
		uitnodiging.setVerstuurdDatum(date.minusSeconds(20).toDate());
		hibernateService.saveOrUpdate(uitnodiging);

		uitnodiging = geefUitnodiging(screeningRonde);
		geefIFobttest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(uitnodiging);
	}

	@Override
	public void maakClientKlaarVoorRappeleren(GbaPersoon filter)
	{
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		hibernateService.saveOrUpdate(client);
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		ColonUitnodiging uitnodiging = geefUitnodiging(screeningRonde);
		geefIFobttest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(uitnodiging);
	}

	@Override
	public void maakClientKlaarVoorAfronden(GbaPersoon filter)
	{
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		hibernateService.saveOrUpdate(client);
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);

		DateTime date = currentDateSupplier.getDateTime();
		int uitnodigingsDagen = simplePreferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		date = date.minusDays(uitnodigingsDagen + 1);
		Date uitnodigingsDate = date.toDate();

		if (screeningRonde.getCreatieDatum() == null)
		{
			screeningRonde.setCreatieDatum(uitnodigingsDate);
		}
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		ColonUitnodiging uitnodiging = geefUitnodiging(screeningRonde);
		IFOBTTest test = geefIFobttest(uitnodiging, screeningRonde);
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

		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		hibernateService.saveOrUpdate(client);
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		hibernateService.saveOrUpdate(dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(screeningRonde);
		ColonUitnodiging uitnodiging = geefUitnodiging(screeningRonde);

		if (uitnodiging.getGekoppeldeTest() != null)
		{
			uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.STANDAARD);
		}

		IFOBTTest test = geefIFobttest(uitnodiging, screeningRonde);

		test.setNormWaarde(normwaarde);
		test.setUitslag(uitslag);

		if (test.getStatus() == IFOBTTestStatus.ACTIEF && !IFOBTTestUtil.isOngunstig(test))
		{
			test.setStatus(IFOBTTestStatus.UITGEVOERD);
		}
		else if (test.getStatus() == IFOBTTestStatus.ACTIEF || IFOBTTestUtil.isOngunstig(test))
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
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		hibernateService.saveOrUpdate(client);
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		hibernateService.saveOrUpdate(dossier);
		ColonScreeningRonde screeningRonde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(screeningRonde);

		ColonUitnodiging uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.VERGELIJKEND);
		hibernateService.saveOrUpdate(uitnodiging);
		IFOBTTest test = geefIFobttest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(test);
		IFOBTTest studietest = geefStudietest(uitnodiging, screeningRonde);
		hibernateService.saveOrUpdate(studietest);
		hibernateService.saveOrUpdateAll(dossier, screeningRonde, client);
	}

	private ColonUitnodiging maakNieuweUitnodiging(ColonScreeningRonde screeningRonde, ColonOnderzoeksVariant onderzoeksVariant)
	{
		ColonUitnodiging uitnodiging = new ColonUitnodiging();
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
		ColonUitnodiging uitnodiging = screeningRonde.getLaatsteUitnodiging();
		if (uitnodiging == null)
		{
			uitnodiging = maakNieuweUitnodiging(screeningRonde, ColonOnderzoeksVariant.STANDAARD);
		}

		return uitnodiging;
	}

	private IFOBTTest geefIFobttest(ColonUitnodiging uitnodiging, ColonScreeningRonde screeningRonde)
	{
		IFOBTTest ifobt = uitnodiging.getGekoppeldeTest();
		if (ifobt == null)
		{
			ifobt = new IFOBTTest();
			LocalDateTime date = currentDateSupplier.getLocalDateTime();
			int rapelDagen = simplePreferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			date = date.minusDays(rapelDagen + 1);
			Date rapelDate = DateUtil.toUtilDate(date);
			ifobt.setBarcode("TGD" + IFOBTTestUtil.getIfobtTestBarcode(uitnodiging.getUitnodigingsId()));
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
			ColonVooraankondiging colonVooraankondiging = screeningRonde.getDossier().getColonVooraankondiging();
			colonVooraankondiging.setCreatieDatum(DateUtil.toUtilDate(date.minusDays(14)));
			hibernateService.saveOrUpdate(colonVooraankondiging);
		}
		return ifobt;
	}

	private IFOBTTest geefStudietest(ColonUitnodiging uitnodiging, ColonScreeningRonde screeningRonde)
	{
		IFOBTTest studietest = new IFOBTTest();
		studietest.setBarcode("TST" + IFOBTTestUtil.getIfobtTestBarcode(uitnodiging.getUitnodigingsId()));
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
		ColonScreeningRonde screeningRonde = dossier.getLaatsteScreeningRonde();
		if (screeningRonde == null)
		{
			screeningRonde = maakNieuweScreeningRonde(dossier);
		}
		return screeningRonde;
	}

	@Override
	public ColonScreeningRonde maakNieuweScreeningRonde(ColonDossier dossier)
	{
		ColonScreeningRonde screeningRonde = new ColonScreeningRonde();
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
	public void huisartsBerichtKlaarzettten(GbaPersoon filter, HuisartsBerichtType berichtType)
	{
		Client client = geefClient(filter.getBsn(), filter.getGeboortedatum(), filter.getOverlijdensdatum());
		geefAdres(client, filter.getGbaAdres().getGbaGemeente());
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		ColonDossier dossier = client.getColonDossier();
		maakVooraankonding(client, dossier);
		ColonScreeningRonde ronde = geefScreeningRonde(dossier);
		hibernateService.saveOrUpdate(client);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(ronde);

		MailMergeContext context = new MailMergeContext();
		context.setClient(client);

		huisartsBerichtService.verstuurColonHuisartsBericht(client, ronde, berichtType, context);
	}

	private Client geefClient(String bsn, Date geboortedatum, Date overlijdensDatum)
	{
		Client client = clientDao.getClientByBsn(bsn);
		if (client == null)
		{
			client = new Client();
			GbaPersoon persoon = new GbaPersoon();
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
			GbaPersoon persoon = client.getPersoon();
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
		BagAdres gbaAdres = client.getPersoon().getGbaAdres();
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
			String postcode = "1111XX";
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
		for (int i = 0; i < aantal; i++)
		{
			GbaPersoon persoon = new GbaPersoon();
			persoon.setGeslacht(Geslacht.MAN);
			persoon.setGbaAdres(new BagAdres());
			persoon.setBsn(TestBsnGenerator.getValideBsn());
			maakClientKlaarVoorRappeleren(persoon);
			hibernateService.getHibernateSession().flush();
			Client client = geefClient(persoon.getBsn(), null, null);
			ColonScreeningRonde ronde = geefScreeningRonde(client.getColonDossier());
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

				String aantalDagenVooraankondigingCell = columns[headersIndex.get("DatumVooraankondiging")];
				if (StringUtils.isNotBlank(aantalDagenVooraankondigingCell) && !aantalDagenVooraankondigingCell.equals("0"))
				{
					ColonDossier colonDossier = new ColonDossier();
					colonDossier.setStatus(DossierStatus.ACTIEF);
					colonDossier.setAangemeld(true);
					colonDossier.setClient(client);
					client.setColonDossier(colonDossier);

					DateTime datumVooraankondiging = currentDateSupplier.getDateTime();

					Integer aantalDagenVooraankondiging = Integer.parseInt(aantalDagenVooraankondigingCell) + startRondeCorrectie;
					datumVooraankondiging = datumVooraankondiging.plusDays(aantalDagenVooraankondiging);

					String statusRonde = columns[headersIndex.get("StatusRonde")];
					if (StringUtils.isNotBlank(statusRonde))
					{
						ColonVooraankondiging colonVooraankondiging = new ColonVooraankondiging();
						ColonBrief brief = new ColonBrief();
						colonVooraankondiging.setBrief(brief);
						colonVooraankondiging.setClient(client);
						colonVooraankondiging.setCreatieDatum(datumVooraankondiging.toDate());

						brief.setBriefType(BriefType.COLON_VOORAANKONDIGING);
						brief.setCreatieDatum(datumVooraankondiging.toDate());
						brief.setGegenereerd(true);
						brief.setTemplateNaam("vooraankondiging.doc");
						brief.setClient(client);

						ColonMergedBrieven mergedBrieven = new ColonMergedBrieven();
						mergedBrieven.setBriefType(BriefType.COLON_VOORAANKONDIGING);
						mergedBrieven.setCreatieDatum(datumVooraankondiging.toDate());
						mergedBrieven.setGeprint(true);
						mergedBrieven.setPrintDatum(datumVooraankondiging.toDate());
						mergedBrieven.setVerwijderd(true);
						brief.setMergedBrieven(mergedBrieven);
						mergedBrieven.setBrieven(new ArrayList<ColonBrief>());
						mergedBrieven.getBrieven().add(brief);
						colonDossier.setColonVooraankondiging(colonVooraankondiging);

						ColonUitnodigingCategorie colonUitnodigingCategorie = null;
						String cat = columns[headersIndex.get(" CAT ")];
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
						DateTime datumRondeDatum = currentDateSupplier.getDateTime();
						Integer aantalDagendatumRondeDatum = Integer.parseInt(columns[headersIndex.get("DatumRonde")]) + startRondeCorrectie;
						datumRondeDatum = datumRondeDatum.plusDays(aantalDagendatumRondeDatum);

						ColonScreeningRonde colonScreeningRonde = new ColonScreeningRonde();
						colonScreeningRonde.setCreatieDatum(datumRondeDatum.toDate());
						colonScreeningRonde.setStatus(ScreeningRondeStatus.valueOf(statusRonde));
						colonScreeningRonde.setStatusDatum(datumRondeDatum.toDate());
						colonScreeningRonde.setDossier(colonDossier);
						colonScreeningRonde.setAangemeld(true);
						colonDossier.getScreeningRondes().add(colonScreeningRonde);
						colonDossier.setLaatsteScreeningRonde(colonScreeningRonde);
						brief.setScreeningRonde(colonScreeningRonde);

						ColonUitnodiging uitnodiging = new ColonUitnodiging();
						uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
						uitnodiging.setScreeningRonde(colonScreeningRonde);
						uitnodiging.setColonUitnodigingCategorie(colonUitnodigingCategorie);
						uitnodiging.setCreatieDatum(datumRondeDatum.toDate());
						uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.STANDAARD);
						String ifobtOntvangen = columns[headersIndex.get("IFOBT ontvangen")];
						if (StringUtils.isNotBlank(columns[headersIndex.get("Uitnodiging")]))
						{
							uitnodiging.setUitnodigingsDatum(datumRondeDatum.toDate());
							uitnodiging.setVerstuurd(true);
							uitnodiging.setVerstuurdDatum(datumRondeDatum.minusDays(2).toDate());
						}
						else
						{
							uitnodiging.setUitnodigingsDatum(datumRondeDatum.plusDays(10).toDate());
							uitnodiging.setVerstuurd(false);
						}
						colonScreeningRonde.setLaatsteUitnodiging(uitnodiging);
						colonScreeningRonde.getUitnodigingen().add(uitnodiging);
						colonScreeningRonde.getBrieven().add(brief);
						colonScreeningRonde.setLaatsteBrief(brief);

						if (StringUtils.isNotBlank(columns[headersIndex.get("Uitnodiging")]))
						{
							IFOBTTest ifobtTest = new IFOBTTest();
							if (colonUitnodigingCategorie == ColonUitnodigingCategorie.U3)
							{
								ifobtTest.setRedenNietTeBeoordelen(RedenNietTeBeoordelen.BARCODE_ONLEESBAAR);
							}
							colonScreeningRonde.getIfobtTesten().add(ifobtTest);
							colonScreeningRonde.setLaatsteIFOBTTest(ifobtTest);
							ifobtTest.setColonScreeningRonde(colonScreeningRonde);
							ifobtTest.setStatusDatum(datumRondeDatum.minusSeconds(1).toDate());
							ifobtTest.setBarcode("AAB" + StringUtils.leftPad("" + trackTraceId++, 4, "0"));
							ifobtTest.setDatumVerstuurd(datumRondeDatum.minusDays(12).toDate());
							ifobtTest.setType(IFOBTType.GOLD);
							uitnodiging.setVerstuurdDoorInpakcentrum(true);
							uitnodiging.setGekoppeldeTest(ifobtTest);

							uitnodiging.setTrackTraceId("" + trackTraceId);
							ifobtTest.setColonUitnodiging(uitnodiging);

							if (StringUtils.isNotBlank(ifobtOntvangen))
							{
								ifobtTest.setVerwerkingsDatum(datumRondeDatum.minusDays(12).toDate());
								ifobtTest.setAnalyseDatum(datumRondeDatum.minusDays(11).toDate());
								ifobtTest.setUitslag(BigDecimal.valueOf(Integer.parseInt(columns[headersIndex.get("IFOBT uitslag")])));
								ifobtTest.setNormWaarde(BigDecimal.valueOf(Integer.parseInt(columns[headersIndex.get("IFOBT norm")])));
								String statusTest = columns[headersIndex.get("StatusTest")];
								if (StringUtils.isNotBlank(statusTest))
								{
									IFOBTTestStatus testStatus = IFOBTTestStatus.valueOf(statusTest);
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
		int gevondenEnIsVerwijderd = 0;
		String[] bsnList = bsns.split(",");
		String result = "Succesvol";
		for (String bsn : bsnList)
		{
			try
			{
				if (StringUtils.isBlank(bsn) || bsn.trim().length() != 9)
				{
					continue;
				}
				Client client = clientDao.getClientByBsn(bsn.trim());
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
		ColonDossier dossier = client.getColonDossier();
		if (dossier == null)
		{
			return;
		}

		verwijderRondesVanColonDossier(dossier);
		hibernateService.deleteAll(client.getAfspraken());
		client.setAfspraken(new ArrayList<>());

		hibernateService.deleteAll(client.getComplicaties());
		if (dossier.getColonVooraankondiging() != null)
		{
			ColonVooraankondiging vooraankondigiging = dossier.getColonVooraankondiging();
			dossier.setColonVooraankondiging(null);
			hibernateService.delete(vooraankondigiging);
		}

		List<ColonVooraankondiging> vooraankondigingen = hibernateService.getByParameters(ColonVooraankondiging.class, ImmutableMap.of("client", client));
		hibernateService.deleteAll(vooraankondigingen);

		for (ColonAfmelding afmelding : dossier.getAfmeldingen())
		{
			if (ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(afmelding.getReden()))
			{
				if (clientService.isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(afmelding.getHandtekeningDocumentAfmelding(), "handtekeningDocumentAfmelding"))
				{
					afmelding.setHandtekeningDocumentAfmelding(null);
					hibernateService.saveOrUpdate(afmelding);
				}
				if (clientService.isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(afmelding.getHandtekeningDocumentHeraanmelding(), "handtekeningDocumentHeraanmelding"))
				{
					afmelding.setHandtekeningDocumentHeraanmelding(null);
					hibernateService.saveOrUpdate(afmelding);
				}
			}
		}

		client.setColonDossier(null);
		hibernateService.saveOrUpdate(client);

		hibernateService.delete(dossier);

		List<ColonBrief> overgeblevenBrieven = hibernateService.getByParameters(ColonBrief.class, ImmutableMap.of("client", client));
		hibernateService.deleteAll(overgeblevenBrieven);

		dossierFactory.maakDossiers(client);
	}

	private void verwijderRondesVanColonDossier(ColonDossier dossier)
	{
		List<ColonScreeningRonde> rondes = dossier.getScreeningRondes();
		dossier.setLaatsteScreeningRonde(null);
		dossier.setScreeningRondes(new ArrayList<>());
		for (ColonScreeningRonde ronde : rondes)
		{
			for (ColonBrief brief : ronde.getBrieven())
			{
				brief.setIfobtTest(null);
				hibernateService.saveOrUpdate(brief);
			}
			verwijderIFOBTestVoorUitnodigingen(ronde.getUitnodigingen());
		}
		hibernateService.deleteAll(rondes);
	}

	private void verwijderIFOBTestVoorUitnodigingen(List<ColonUitnodiging> uitnodigingen)
	{
		for (ColonUitnodiging uitnodiging : uitnodigingen)
		{
			IFOBTTest test = uitnodiging.getGekoppeldeTest();
			IFOBTTest testE = uitnodiging.getGekoppeldeExtraTest();
			uitnodiging.setGekoppeldeTest(null);
			uitnodiging.setGekoppeldeExtraTest(null);
			hibernateService.saveOrUpdate(uitnodiging);

			if (test != null)
			{
				ColonScreeningRonde andereScreeningRonde = test.getColonScreeningRonde();
				andereScreeningRonde.setLaatsteIFOBTTest(null);
				andereScreeningRonde.setLaatsteIFOBTTestExtra(null);
				andereScreeningRonde.getIfobtTesten().remove(test);
				hibernateService.saveOrUpdate(andereScreeningRonde);
				hibernateService.delete(test);
			}
			if (testE != null)
			{
				ColonScreeningRonde andereScreeningRonde = testE.getColonScreeningRonde();
				andereScreeningRonde.setLaatsteIFOBTTest(null);
				andereScreeningRonde.setLaatsteIFOBTTestExtra(null);
				andereScreeningRonde.getIfobtTesten().remove(testE);
				hibernateService.saveOrUpdate(andereScreeningRonde);
				hibernateService.delete(testE);
			}
		}
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

		int aantalRoosterItemsVerwijderd = 0;

		List<Long> alleRoosterBlokken = hibernateService.getHibernateSession().createCriteria(RoosterItem.class).add(Restrictions.isNotEmpty("afspraken"))
			.setProjection(Projections.id()).list();

		for (Long roosterBlokId : alleRoosterBlokken)
		{
			RoosterItem roosterBlok = hibernateService.get(RoosterItem.class, roosterBlokId);
			if (!roosterBlok.getAfspraken().isEmpty())
			{
				for (Afspraak afspraak : roosterBlok.getAfspraken())
				{
					afspraak.setRoosterItem(null);
					hibernateService.saveOrUpdate(afspraak);
				}
				roosterBlok.getAfspraken().clear();
				hibernateService.saveOrUpdate(roosterBlok);
			}
		}
		List<Long> alleRecurrences = hibernateService.getHibernateSession().createCriteria(AbstractRecurrence.class).setProjection(Projections.id()).list();
		for (Long recurrenceId : alleRecurrences)
		{
			AbstractRecurrence recurrence = hibernateService.get(AbstractRecurrence.class, recurrenceId);
			if (recurrence != null && !NoRecurrence.class.isAssignableFrom(recurrence.getClass()))
			{
				aantalRoosterItemsVerwijderd += recurrence.getAppointments().size();
				recurrenceService.deleteHerhalingChain(recurrence);
			}
		}

		alleRoosterBlokken = hibernateService.getHibernateSession().createCriteria(RoosterItem.class).add(Restrictions.isNull("recurrence")).setProjection(Projections.id()).list();
		for (Long roosterBlokId : alleRoosterBlokken)
		{
			RoosterItem roosterBlok = hibernateService.get(RoosterItem.class, roosterBlokId);
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
			DateTime datumVooraankondiging = new DateTime(client.getColonDossier().getColonVooraankondiging().getCreatieDatum());
			DateTime datumVooraankondigingPlusPeriode = datumVooraankondiging.plusDays(simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name()));
			if (datumVooraankondigingPlusPeriode.isBeforeNow() && client.getColonDossier().getScreeningRondes().size() == 0)
			{
				return ColonUitnodigingCategorie.U1;
			}

			if (client.getColonDossier().getLaatsteScreeningRonde() != null)
			{
				LocalDate datumVolgendeUitnodiging = dossierService.getDatumVolgendeUitnodiging(client.getColonDossier());
				boolean volgendeUitnodigingsDatumBereikt = !datumVolgendeUitnodiging.isAfter(LocalDate.now());
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
