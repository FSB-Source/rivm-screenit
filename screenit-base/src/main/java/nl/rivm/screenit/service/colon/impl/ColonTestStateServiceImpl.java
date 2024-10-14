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

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
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
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonTestStateService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@Slf4j
public class ColonTestStateServiceImpl implements ColonTestStateService
{
	@Autowired
	private HibernateService hibernateService;

	private static final Integer AANTALDAGENNAUITNODIGING = 55;

	@Autowired
	private ClientService clientService;

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

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Override
	public String setClientInState(TestModel model)
	{

		Client client = geefClient(model.getBsn(), model.getGeboortedatum(), model.getGbaStatus(), model.getDatumOverlijden(), model.getGeslacht());
		geefAdres(client, model.getGemeente());
		ColonDossier dossier = client.getColonDossier();
		var nu = currentDateSupplier.getDate();

		String melding = "Geen testacties geselecteerd.";
		if (model.getColonTestActies() != null)
		{
			melding = "Client in status " + model.getColonTestActies().toString() + " gezet. Colon client selectie job kan gedraaid worden.";
		}

		var nuMinAantalDagenUitnodiging = DateUtil.minDagen(nu, AANTALDAGENNAUITNODIGING);

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
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU2_2(dossier, ColonUitnodigingCategorie.U2);
				break;
			case U2_3:
				setClientInU2_3(dossier, ColonUitnodigingCategorie.U2);
				break;
			case U3_1:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.GEEN_MONSTER);
				break;
			case U3_2:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.BARCODE_ONLEESBAAR);
				break;
			case U3_3:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.BUIS_KAPOT);
				break;
			case U3_4:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.GEEN_VLOEISTOF);
				break;
			case U3_5:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.TECHNISCH_ONMOGELIJK);
				break;
			case U3_6:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.TE_WEINIG_ONTLASTING);
				break;
			case U3_7:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU3Default(dossier, RedenNietTeBeoordelen.TE_VEEL_ONTLASTING);
				break;
			case U4:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU4(dossier);
				break;
			case U6:
				dossier = setClientInU1(dossier, true, nuMinAantalDagenUitnodiging, null);
				uitnodigingVerstuurd(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging(), nuMinAantalDagenUitnodiging);
				setClientInU6(dossier);
				break;
			}
		}
		return melding;
	}

	private ColonDossier setClientInU1(ColonDossier dossier, boolean totUitnodiging, Date nu, ColonUitnodigingCategorie u)
	{
		Client client = dossier.getClient();
		Date vooraankondigingdate;
		Date uitnodigingsdate;
		Integer dagen = simplePreferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
		if (totUitnodiging)
		{
			vooraankondigingdate = DateUtil.minDagen(nu, dagen);
			uitnodigingsdate = nu;
		}
		else
		{
			vooraankondigingdate = nu;
			uitnodigingsdate = DateUtil.plusDagen(nu, dagen);
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
		var nu = currentDateSupplier.getDate();
		fixU2_2Dossier(dossier, nu);
		ColonScreeningRonde ronde = newScreeningRonde(dossier, nu);
		ColonUitnodiging uitnodiging = nieuweUitnodiging(ronde, nu, u);
		hibernateService.saveOrUpdateAll(uitnodiging, ronde);
		return dossier;
	}

	private ColonDossier setClientInU2_3(ColonDossier dossier, ColonUitnodigingCategorie u)
	{
		var nu = currentDateSupplier.getDate();
		ColonScreeningRonde ronde = newScreeningRonde(dossier, nu);
		ColonUitnodiging uitnodiging = nieuweUitnodiging(ronde, nu, u);
		dossier.setColonVooraankondiging(null);
		hibernateService.saveOrUpdateAll(ronde, dossier, uitnodiging);
		return dossier;
	}

	private void setClientInU3Default(ColonDossier dossier, RedenNietTeBeoordelen reden)
	{
		var nu = currentDateSupplier.getDate();
		fixU3Dossier(dossier, reden, DateUtil.minDagen(nu, 31));
	}

	private void setClientInU4(ColonDossier dossier)
	{
		var nu = currentDateSupplier.getDate();
		fixU4Dossier(dossier, nu);
	}

	private void setClientInU6(ColonDossier dossier)
	{
		var nu = currentDateSupplier.getDate();
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
			var nu = currentDateSupplier.getDate();
			ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(nu);
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
			ifobt.setBarcode(FITTestUtil.getFITTestBarcode());
			ifobt.setType(IFOBTType.GOLD);
			ifobt.setStatus(IFOBTTestStatus.ACTIEF);
			ifobt.setHerinnering(Boolean.FALSE);
			ifobt.setDatumVerstuurd(nu);
			ifobt.setStatusDatum(nu);
			ifobt.setColonScreeningRonde(screeningRonde);
			ifobt.setColonUitnodiging(uitnodiging);

			uitnodiging.setGekoppeldeTest(ifobt);
			uitnodiging.setVerstuurd(true);
			uitnodiging.setVerstuurdDatum(currentDateSupplier.getDate());
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

	private ColonDossier fixU2_2Dossier(ColonDossier dossier, Date nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.UITGEVOERD);
			test.setStatusDatum(DateUtil.minDagen(nu, 8));
			test.setVerwerkingsDatum(DateUtil.minDagen(nu, 15));
			test.setNormWaarde(new BigDecimal("25.5"));
			test.setUitslag(new BigDecimal("100.5"));
			hibernateService.saveOrUpdate(test);

			maakAfspraakEnConclusie(dossier);

			ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(nu);
			hibernateService.saveOrUpdate(ronde);
		}
		return dossier;
	}

	private ColonDossier fixU3Dossier(ColonDossier dossier, RedenNietTeBeoordelen reden, Date nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			ColonUitnodiging uitnodiging = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
			uitnodiging.setVerstuurdDatum(DateUtil.minDagen(uitnodiging.getVerstuurdDatum(), 5));

			hibernateService.saveOrUpdate(uitnodiging);

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.NIETTEBEOORDELEN);
			test.setRedenNietTeBeoordelen(reden);
			test.setStatusDatum(nu);
			test.setVerwerkingsDatum(nu);

			hibernateService.saveOrUpdate(test);

			ColonVooraankondiging voor = dossier.getColonVooraankondiging();
			voor.setCreatieDatum(DateUtil.minDagen(voor.getCreatieDatum(), 5));
			hibernateService.saveOrUpdate(voor);
		}
		return dossier;
	}

	private ColonDossier fixU4Dossier(ColonDossier dossier, Date nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.VERLOREN);
			test.setStatusDatum(nu);
			hibernateService.saveOrUpdate(test);

			ColonUitnodiging uitnodigingIFOBTEN = FITTestUtil.getUitnodiging(test);
			IFOBTTest studietest = uitnodigingIFOBTEN.getGekoppeldeExtraTest();
			if (studietest != null && studietest.getType().equals(IFOBTType.STUDIE))
			{
				studietest.setStatus(IFOBTTestStatus.VERLOREN);
				studietest.setStatusDatum(nu);
				hibernateService.saveOrUpdate(studietest);
			}

			ColonVooraankondiging voor = dossier.getColonVooraankondiging();
			voor.setCreatieDatum(DateUtil.minDagen(voor.getCreatieDatum(), 5));
			hibernateService.saveOrUpdate(voor);

			ColonUitnodiging uitnodiging = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
			uitnodiging.setVerstuurdDatum(DateUtil.minDagen(uitnodiging.getVerstuurdDatum(), 5));

			hibernateService.saveOrUpdate(uitnodiging);
		}
		return dossier;
	}

	private ColonDossier fixU6Dossier(ColonDossier dossier, Date nu)
	{
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{

			IFOBTTest test = dossier.getLaatsteScreeningRonde().getLaatsteIFOBTTest();
			test.setStatus(IFOBTTestStatus.VERVALDATUMVERLOPEN);
			test.setStatusDatum(nu);
			hibernateService.saveOrUpdate(test);

			ColonVooraankondiging voor = dossier.getColonVooraankondiging();
			voor.setCreatieDatum(DateUtil.minDagen(voor.getCreatieDatum(), 5));

			hibernateService.saveOrUpdate(voor);

			ColonUitnodiging uitnodiging = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
			uitnodiging.setVerstuurdDatum(DateUtil.minDagen(uitnodiging.getVerstuurdDatum(), 5));
			hibernateService.saveOrUpdate(uitnodiging);
		}
		return dossier;
	}

	private Client geefClient(String bsn, Date geboortedatum, GbaStatus gbaStatus, Date datumOverlijden, Geslacht geslacht)
	{
		Client client = clientService.getClientByBsn(bsn);
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
			intakeAfspraak.setStatus(ColonAfspraakStatus.GEPLAND);
			intakeAfspraak.setAfstand(BigDecimal.valueOf(2.3));
			intakeAfspraak.setBezwaar(false);
			intakeAfspraak.setClient(client);
			intakeAfspraak.setAangemaaktOp(currentDateSupplier.getLocalDateTime());
			intakeAfspraak.setGewijzigdOp(currentDateSupplier.getLocalDateTime());
			client.getAfspraken().add(intakeAfspraak);
			screeningRonde.setLaatsteAfspraak(intakeAfspraak);
			screeningRonde.getAfspraken().add(intakeAfspraak);
			intakeAfspraak.setColonScreeningRonde(screeningRonde);

		}
		intakeAfspraak.setBezwaar(false);
		intakeAfspraak.setStatus(ColonAfspraakStatus.GEPLAND);
		if (intakeAfspraak.getVanaf() == null)
		{
			intakeAfspraak.setVanaf(currentDateSupplier.getLocalDateTime().plusDays(1));
		}
		if (intakeAfspraak.getKamer() == null)
		{
			List<ColonIntakekamer> all = hibernateService.loadAll(ColonIntakekamer.class);
			intakeAfspraak.setKamer(all.get(0));
		}

		if (intakeAfspraak.getTot() == null)
		{
			var duurAfspraakInMinuten = (int) organisatieParameterService.getOrganisatieParameter(intakeAfspraak.getKamer().getIntakelocatie(),
				OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, 15);
			intakeAfspraak.setTot(intakeAfspraak.getVanaf().plusMinutes(duurAfspraakInMinuten));
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
		intakeAfspraak.setStatus(ColonAfspraakStatus.UITGEVOERD);
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
		brief.setCreatieDatum(DateUtil.minDagen(nu, 1));
		brief.setGegenereerd(true);
		brief.setBriefType(briefType);
		brief.setClient(client);
		hibernateService.saveOrUpdate(brief);
		return brief;
	}
}
