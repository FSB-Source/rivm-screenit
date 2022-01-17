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

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.TestModel;
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
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonTestStateService;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.planning.model.Discipline;

import org.hibernate.criterion.Restrictions;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class ColonTestStateServiceImpl implements ColonTestStateService
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonTestStateServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	private static Integer AANTALDAGENNAUITNODIGING = 55;

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private AfspraakDefinitieDao afspraakDefinitieDao;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private ColonDossierBaseService dossierService;

	@Override
	public String setClientInState(TestModel model)
	{

		Client client = geefClient(model.getBsn(), model.getGeboortedatum(), model.getGbaStatus(), model.getDatumOverlijden(), model.getGeslacht());
		geefAdres(client, model.getGemeente());
		ColonDossier dossier = client.getColonDossier();
		DateTime nu = currentDateSupplier.getDateTime();

		String melding = "Geen testacties geselecteerd.";
		if (model.getColonTestActies() != null)
		{
			melding = "Client in status " + model.getColonTestActies().toString() + " gezet. Colon client selectie job kan gedraaid worden.";
		}

		if (model.getColonTestActies() != null)
		{
			switch (model.getColonTestActies())
			{
			case U1:
				setClientInU1(dossier, false, nu, ColonUitnodigingCategorie.U1);
				break;
			case U2_1:
				setClientInU1(dossier, true, nu, ColonUitnodigingCategorie.U1);
				break;
			case U2_2:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU2_2(dossier, ColonUitnodigingCategorie.U2);
				break;
			case U2_3:
				setClientInU2_3(dossier, ColonUitnodigingCategorie.U2);
				break;
			case U3_1:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.GEEN_MONSTER);
				break;
			case U3_2:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.BARCODE_ONLEESBAAR);
				break;
			case U3_3:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.BUIS_KAPOT);
				break;
			case U3_4:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.GEEN_VLOEISTOF);
				break;
			case U3_5:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.TECHNISCH_ONMOGELIJK);
				break;
			case U3_6:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.TE_WEINIG_ONTLASTING);
				break;
			case U3_7:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU3Default(dossier, RedenNietTeBeoordelen.TE_VEEL_ONTLASTING);
				break;
			case U4:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU4(dossier);
				break;
			case U6:
				dossier = setClientInU1(dossier, true, nu.minusDays(AANTALDAGENNAUITNODIGING), null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nu.minusDays(AANTALDAGENNAUITNODIGING).toDate());
				setClientInU6(dossier);
				break;
			}
		}
		return melding;
	}

	private ColonDossier setClientInU1(ColonDossier dossier, boolean totUitnodiging, DateTime nu, ColonUitnodigingCategorie u)
	{
		Client client = dossier.getClient();
		Date vooraankondigingdate;
		Date uitnodigingsdate;
		Integer dagen = simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
		if (totUitnodiging)
		{
			vooraankondigingdate = nu.minusDays(dagen).toDate();
			uitnodigingsdate = nu.toDate();
		}
		else
		{
			vooraankondigingdate = nu.toDate();
			uitnodigingsdate = nu.plusDays(dagen).toDate();
		}

		ColonVooraankondiging voor = dossier.getColonVooraankondiging();
		if (voor == null)
		{
			voor = new ColonVooraankondiging();
			voor.setClient(client);
			voor.setCreatieDatum(vooraankondigingdate);
			voor.setBrief(maakBrief("vooraankondigingbrief.docx", BriefType.COLON_VOORAANKONDIGING, client, vooraankondigingdate));
		}
		dossier.setColonVooraankondiging(voor);
		hibernateService.saveOrUpdate(voor);
		hibernateService.saveOrUpdate(dossier);

		ColonScreeningRonde ronde = newScreeningRonde(dossier, vooraankondigingdate);
		ColonUitnodiging uitnodiging = nieuweUitnodiging(ronde, uitnodigingsdate, u);
		hibernateService.saveOrUpdate(uitnodiging);
		hibernateService.saveOrUpdate(ronde);
		return dossier;
	}

	private ColonDossier setClientInU2_2(ColonDossier dossier, ColonUitnodigingCategorie u)
	{
		DateTime nu = currentDateSupplier.getDateTime();
		fixU2_2Dossier(dossier, nu);
		ColonScreeningRonde ronde = newScreeningRonde(dossier, nu.toDate());
		ColonUitnodiging uitnodiging = nieuweUitnodiging(ronde, nu.toDate(), u);
		hibernateService.saveOrUpdateAll(uitnodiging, ronde);
		return dossier;
	}

	private ColonDossier setClientInU2_3(ColonDossier dossier, ColonUitnodigingCategorie u)
	{
		DateTime nu = currentDateSupplier.getDateTime();
		ColonScreeningRonde ronde = newScreeningRonde(dossier, nu.toDate());
		ColonUitnodiging uitnodiging = nieuweUitnodiging(ronde, nu.toDate(), u);
		dossier.setColonVooraankondiging(null);
		hibernateService.saveOrUpdateAll(ronde, dossier, uitnodiging);
		return dossier;
	}

	private void setClientInU3Default(ColonDossier dossier, RedenNietTeBeoordelen reden)
	{
		DateTime nu = currentDateSupplier.getDateTime();
		fixU3Dossier(dossier, reden, nu.minusDays(31));
	}

	private void setClientInU4(ColonDossier dossier)
	{
		DateTime nu = currentDateSupplier.getDateTime();
		fixU4Dossier(dossier, nu);
	}

	private void setClientInU6(ColonDossier dossier)
	{
		DateTime nu = currentDateSupplier.getDateTime();
		fixU6Dossier(dossier, nu);
	}

	private ColonScreeningRonde newScreeningRonde(ColonDossier dossier, Date nu)
	{
		closeOtherScreeningRondes(dossier);
		ColonScreeningRonde screeningRonde = new ColonScreeningRonde();
		screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
		screeningRonde.setAangemeld(true);
		screeningRonde.setCreatieDatum(nu);
		dossier.setLaatsteScreeningRonde(screeningRonde);
		dossier.getScreeningRondes().add(screeningRonde);
		screeningRonde.setDossier(dossier);
		hibernateService.saveOrUpdate(dossier);
		hibernateService.saveOrUpdate(screeningRonde);
		return screeningRonde;
	}

	private void closeOtherScreeningRondes(ColonDossier dossier)
	{
		if (dossier.getLaatsteScreeningRonde() != null)
		{
			DateTime nu = currentDateSupplier.getDateTime();
			ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(nu.toDate());
			hibernateService.saveOrUpdate(ronde);
		}
	}

	private ColonUitnodiging nieuweUitnodiging(ColonScreeningRonde screeningRonde, Date nu, ColonUitnodigingCategorie u)
	{
		ColonUitnodiging uitnodiging = new ColonUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		uitnodiging.setCreatieDatum(nu);
		uitnodiging.setScreeningRonde(screeningRonde);
		uitnodiging.setUitnodigingsDatum(nu);
		uitnodiging.setVerstuurd(false);
		uitnodiging.setTemplateNaam(briefService.getNieuwsteBriefDefinitie(BriefType.COLON_UITNODIGING).getDocument().getNaam());

		if (u == null)
		{
			u = ColonUitnodigingCategorie.U1;

			IFOBTTest ifobt = new IFOBTTest();
			ifobt.setBarcode(IFOBTTestUtil.getIfobtTestBarcode());
			ifobt.setType(IFOBTType.GOLD);
			ifobt.setStatus(IFOBTTestStatus.ACTIEF);
			ifobt.setHerinnering(Boolean.FALSE);
			ifobt.setDatumVerstuurd(nu);
			ifobt.setStatusDatum(nu);
			ifobt.setColonScreeningRonde(screeningRonde);
			ifobt.setColonUitnodiging(uitnodiging);

			uitnodiging.setGekoppeldeTest(ifobt);
			uitnodiging.setVerstuurd(true);
			uitnodiging.setVerstuurdDatum(new Date());
			uitnodiging.setVerstuurdDoorInpakcentrum(true);

			screeningRonde.setLaatsteIFOBTTest(ifobt);
			screeningRonde.getIfobtTesten().add(ifobt);
			hibernateService.saveOrUpdate(ifobt);
		}

		uitnodiging.setColonUitnodigingCategorie(u);
		uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.STANDAARD);

		screeningRonde.setLaatsteUitnodiging(uitnodiging);
		screeningRonde.getUitnodigingen().add(uitnodiging);

		hibernateService.saveOrUpdateAll(screeningRonde, uitnodiging);
		return uitnodiging;
	}

	private ColonDossier fixU2_2Dossier(ColonDossier dossier, DateTime nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.UITGEVOERD);
			test.setStatusDatum(nu.minusDays(8).toDate());
			test.setVerwerkingsDatum(nu.minusDays(15).toDate());
			test.setNormWaarde(new BigDecimal("25.5"));
			test.setUitslag(new BigDecimal("100.5"));
			hibernateService.saveOrUpdate(test);

			maakAfspraakEnConclusie(dossier);

			ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(nu.toDate());
			hibernateService.saveOrUpdate(ronde);
		}
		return dossier;
	}

	private ColonDossier fixU3Dossier(ColonDossier dossier, RedenNietTeBeoordelen reden, DateTime nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			ColonUitnodiging uitnodiging = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
			uitnodiging.setVerstuurdDatum(new DateTime(uitnodiging.getVerstuurdDatum()).minusDays(5).toDate());

			hibernateService.saveOrUpdate(uitnodiging);

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.NIETTEBEOORDELEN);
			test.setRedenNietTeBeoordelen(reden);
			test.setStatusDatum(nu.toDate());
			test.setVerwerkingsDatum(nu.toDate());

			hibernateService.saveOrUpdate(test);

			ColonVooraankondiging voor = dossier.getColonVooraankondiging();
			DateTime voorcreatieDatum = new DateTime(voor.getCreatieDatum());
			voorcreatieDatum = voorcreatieDatum.minusDays(5);
			voor.setCreatieDatum(voorcreatieDatum.toDate());
			hibernateService.saveOrUpdate(voor);
		}
		return dossier;
	}

	private ColonDossier fixU4Dossier(ColonDossier dossier, DateTime nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.VERLOREN);
			test.setStatusDatum(nu.toDate());
			hibernateService.saveOrUpdate(test);

			ColonUitnodiging uitnodigingIFOBTEN = IFOBTTestUtil.getUitnodiging(test);
			IFOBTTest studietest = uitnodigingIFOBTEN.getGekoppeldeExtraTest();
			if (studietest != null && studietest.getType().equals(IFOBTType.STUDIE))
			{
				studietest.setStatus(IFOBTTestStatus.VERLOREN);
				studietest.setStatusDatum(nu.toDate());
				hibernateService.saveOrUpdate(studietest);
			}

			ColonVooraankondiging voor = dossier.getColonVooraankondiging();
			DateTime voorcreatieDatum = new DateTime(voor.getCreatieDatum());
			voorcreatieDatum = voorcreatieDatum.minusDays(5);
			voor.setCreatieDatum(voorcreatieDatum.toDate());
			hibernateService.saveOrUpdate(voor);

			ColonUitnodiging uitnodiging = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
			uitnodiging.setVerstuurdDatum(new DateTime(uitnodiging.getVerstuurdDatum()).minusDays(5).toDate());

			hibernateService.saveOrUpdate(uitnodiging);
		}
		return dossier;
	}

	private ColonDossier fixU6Dossier(ColonDossier dossier, DateTime nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.VERVALDATUMVERLOPEN);
			test.setStatusDatum(nu.toDate());
			hibernateService.saveOrUpdate(test);

			ColonVooraankondiging voor = dossier.getColonVooraankondiging();
			DateTime voorcreatieDatum = new DateTime(voor.getCreatieDatum());
			voorcreatieDatum = voorcreatieDatum.minusDays(5);
			voor.setCreatieDatum(voorcreatieDatum.toDate());
			hibernateService.saveOrUpdate(voor);

			ColonUitnodiging uitnodiging = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
			uitnodiging.setVerstuurdDatum(new DateTime(uitnodiging.getVerstuurdDatum()).minusDays(5).toDate());
			hibernateService.saveOrUpdate(uitnodiging);
		}
		return dossier;
	}

	private Client geefClient(String bsn, Date geboortedatum, GbaStatus gbaStatus, Date datumOverlijden, Geslacht geslacht)
	{
		Client client = clientDao.getClientByBsn(bsn);
		if (client == null)
		{
			client = new Client();
			GbaPersoon persoon = new GbaPersoon();
			client.setPersoon(persoon);
			persoon.setGeslacht(Geslacht.MAN);
			persoon.setAchternaam("Doe-" + bsn);
			if (Geslacht.MAN.equals(geslacht))
			{
				persoon.setGeslacht(Geslacht.MAN);
				persoon.setVoornaam("John");
			}
			else
			{
				persoon.setGeslacht(Geslacht.VROUW);
				persoon.setVoornaam("Jane");
			}
			persoon.setBsn(bsn);
			if (geboortedatum == null)
			{
				try
				{
					persoon.setGeboortedatum(new SimpleDateFormat("dd-MM-yyyy").parse("01-01-1950"));
				}
				catch (ParseException e)
				{
					LOG.error(e.getMessage(), e);
				}
			}
			else
			{
				persoon.setGeboortedatum(geboortedatum);
			}
			persoon.setPatient(client);
			persoon.setOverlijdensdatum(datumOverlijden);
			client.setGbaStatus(gbaStatus);
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
			persoon.setOverlijdensdatum(datumOverlijden);
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
				gemeente = (Gemeente) hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.isNotNull("screeningOrganisatie")).list().get(0);
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

	private ColonIntakeAfspraak maakAfspraak(ColonDossier dossier)
	{
		Client client = dossier.getClient();
		ColonScreeningRonde screeningRonde = dossier.getLaatsteScreeningRonde();

		ColonIntakeAfspraak intakeAfspraak = screeningRonde.getLaatsteAfspraak();
		if (intakeAfspraak == null)
		{
			intakeAfspraak = new ColonIntakeAfspraak();
			intakeAfspraak.setStatus(AfspraakStatus.GEPLAND);
			intakeAfspraak.setAfstand(BigDecimal.valueOf(2.3));
			intakeAfspraak.setBezwaar(false);
			intakeAfspraak.setAfspraaknummer(System.currentTimeMillis());
			intakeAfspraak.setClient(client);
			intakeAfspraak.setDatumLaatsteWijziging(currentDateSupplier.getDate());
			client.getAfspraken().add(intakeAfspraak);
			screeningRonde.setLaatsteAfspraak(intakeAfspraak);
			screeningRonde.getAfspraken().add(intakeAfspraak);
			intakeAfspraak.setColonScreeningRonde(screeningRonde);
			intakeAfspraak.getDisciplines().add(hibernateService.loadAll(Discipline.class).get(0));

		}
		intakeAfspraak.setBezwaar(false);
		intakeAfspraak.setStatus(AfspraakStatus.GEPLAND);
		if (intakeAfspraak.getStartTime() == null)
		{
			intakeAfspraak.setStartTime(currentDateSupplier.getDateTime().plusDays(1).toDate());
		}
		if (intakeAfspraak.getLocation() == null)
		{
			List<Kamer> all = hibernateService.loadAll(Kamer.class);
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

		hibernateService.saveOrUpdateAll(intakeAfspraak, screeningRonde, client);
		dossierService.setDatumVolgendeUitnodiging(screeningRonde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);
		return intakeAfspraak;
	}

	private ColonConclusie maakAfspraakEnConclusie(ColonDossier dossier)
	{
		ColonIntakeAfspraak intakeAfspraak = maakAfspraak(dossier);
		ColonConclusie conclusie = intakeAfspraak.getConclusie();
		if (conclusie == null)
		{
			conclusie = new ColonConclusie();
			conclusie.setType(ColonConclusieType.COLOSCOPIE);
			conclusie.setDatum(currentDateSupplier.getDate());
			List<InstellingGebruiker> all = hibernateService.loadAll(InstellingGebruiker.class);
			conclusie.setInstellingGebruiker(all.get(0));
			intakeAfspraak.setConclusie(conclusie);
		}
		intakeAfspraak.setStatus(AfspraakStatus.UITGEVOERD);
		hibernateService.saveOrUpdateAll(intakeAfspraak, conclusie);
		return conclusie;
	}

	private void uitnodigingVerstuurd(ColonUitnodiging uitnodiging, Date nu)
	{
		ColonScreeningRonde screeningRonde = uitnodiging.getScreeningRonde();
		uitnodiging.setVerstuurdDatum(nu);
		uitnodiging.setVerstuurdDoorInpakcentrum(Boolean.TRUE);
		uitnodiging.setVerstuurd(Boolean.TRUE);

		ColonBrief brief = maakBrief("2-Brief uitnodiging FIT.doc", BriefType.COLON_UITNODIGING, screeningRonde.getDossier().getClient(), nu);
		ColonMergedBrieven mbrieven = new ColonMergedBrieven();
		mbrieven.setGeprint(true);
		mbrieven.setPrintDatum(nu);
		mbrieven.setBriefType(BriefType.COLON_UITNODIGING);
		brief.setMergedBrieven(mbrieven);
		brief.setScreeningRonde(screeningRonde);
		screeningRonde.getBrieven().add(brief);
		hibernateService.saveOrUpdateAll(mbrieven, brief, uitnodiging, screeningRonde);
	}

	private ColonBrief maakBrief(String templateNaam, BriefType briefType, Client client, Date nu)
	{
		ColonBrief brief = new ColonBrief();
		brief.setTemplateNaam(templateNaam);
		brief.setCreatieDatum(new DateTime(nu).minusDays(1).toDate());
		brief.setGegenereerd(true);
		brief.setBriefType(briefType);
		brief.setClient(client);
		hibernateService.saveOrUpdate(brief);
		return brief;
	}
}
