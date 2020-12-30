package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.concurrent.ThreadLocalRandom;

import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DocumentTemplateTestUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(DocumentTemplateTestUtil.class);

	private static String defaultValue = "xxxxxxxxxxxxxxxx";

	private DocumentTemplateTestUtil()
	{
	}

	public static Client getDefaultFilledClient()
	{
		GbaPersoon persoon = new GbaPersoon();

		persoon.setBsn("999999999");
		persoon.setGeslacht(Geslacht.VROUW);

		try
		{
			persoon.setGeboortedatum(DateUtils.parseDate("31-12-1962", "dd-MM-yyyy"));
		}
		catch (ParseException e)
		{
		}
		persoon.setVoornaam("Anne Bernhard");
		persoon.setTussenvoegsel("van der");
		persoon.setAchternaam("Test");
		persoon.setNaamGebruik(NaamGebruik.EIGEN);

		persoon.setPartnerTussenvoegsel("xxxxxxxxxx");
		persoon.setPartnerAchternaam(defaultValue);

		persoon.setGbaAdres(getDefaultFilledAdres());

		Client client = new Client();
		client.setPersoon(persoon);

		BezwaarMoment bezwaar = new BezwaarMoment();
		client.getBezwaarMomenten().add(bezwaar);

		colonDossier(client);

		mammaDossier(client);

		return client;
	}

	private static void mammaDossier(Client client)
	{
		MammaDossier dossier = new MammaDossier();
		client.setMammaDossier(dossier);

		MammaScreeningRonde screeningRonde = new MammaScreeningRonde();
		dossier.getScreeningRondes().add(screeningRonde);
		dossier.setLaatsteScreeningRonde(screeningRonde);
		screeningRonde.setDossier(dossier);

		MammaUitnodiging uitnodiging = new MammaUitnodiging();
		screeningRonde.getUitnodigingen().add(uitnodiging);
		uitnodiging.setScreeningRonde(screeningRonde);
		screeningRonde.setLaatsteUitnodiging(uitnodiging);

		MammaAfspraak afspraak = new MammaAfspraak();
		afspraak.setUitnodiging(uitnodiging);
		uitnodiging.getAfspraken().add(afspraak);
		uitnodiging.setLaatsteAfspraak(afspraak);

		MammaStandplaatsPeriode standplaatsPeriode = new MammaStandplaatsPeriode();
		afspraak.setStandplaatsPeriode(standplaatsPeriode);

		CentraleEenheid ce = new CentraleEenheid();
		ce.setTelefoon("06-123456789");
		ce.setEmail("info@lijn.nl");
		ce.setClientPortaalVrijeTekst("Vrije tekst client portaal veld");

		BeoordelingsEenheid be = new BeoordelingsEenheid();
		be.setParent(ce);

		MammaScreeningsEenheid se = new MammaScreeningsEenheid();
		se.setBeoordelingsEenheid(be);

		MammaStandplaatsRonde standplaatsRonde = new MammaStandplaatsRonde();
		standplaatsPeriode.setStandplaatsRonde(standplaatsRonde);
		standplaatsRonde.setAchtervangToegepast(false);

		MammaStandplaats standplaats = new MammaStandplaats();
		standplaatsRonde.setStandplaats(standplaats);

		standplaatsPeriode.setScreeningsEenheid(se);

		MammaStandplaatsLocatie locatie = new MammaStandplaatsLocatie();
		locatie.setToonHuisnummerInBrieven(true);
		standplaats.setLocatie(locatie);

		locatie.setStraat("Kerkstraat");
		locatie.setHuisnummer(1);
		locatie.setPlaats("Lutjebroek");
		locatie.setPostcode("8888 WW");

		MammaOnderzoek onderzoek = new MammaOnderzoek();
		afspraak.setOnderzoek(onderzoek);
		onderzoek.setAfspraak(afspraak);

		MammaBeoordeling beoordeling = new MammaBeoordeling();
		beoordeling.setStatus(MammaBeoordelingStatus.UITSLAG_GUNSTIG);
		onderzoek.getBeoordelingen().add(beoordeling);
		onderzoek.setLaatsteBeoordeling(beoordeling);

		MammaLezing lezing1 = new MammaLezing();
		beoordeling.setEersteLezing(lezing1);

		MammaLezing lezing2 = new MammaLezing();
		beoordeling.setTweedeLezing(lezing2);

		Gebruiker radioloog1 = new Gebruiker();
		radioloog1.setOndertekenaar("Radioloog1");
		InstellingGebruiker ilRad1 = new InstellingGebruiker();
		ilRad1.setMedewerker(radioloog1);
		lezing1.setBeoordelaar(ilRad1);
		Gebruiker radioloog2 = new Gebruiker();
		radioloog2.setOndertekenaar("Radioloog2");
		InstellingGebruiker ilRad2 = new InstellingGebruiker();
		ilRad2.setMedewerker(radioloog2);
		lezing2.setBeoordelaar(ilRad2);
		dossier.setLaatsteBeoordelingMetUitslag(beoordeling);
		screeningRonde.setLaatsteOnderzoek(onderzoek);

	}

	private static void colonDossier(Client client)
	{
		ColonDossier dossier = new ColonDossier();
		dossier.setClient(client);
		dossier.setStatus(DossierStatus.ACTIEF);

		ColonScreeningRonde ronde = new ColonScreeningRonde();
		ronde.setDossier(dossier);
		dossier.setLaatsteScreeningRonde(ronde);

		IFOBTTest test = new IFOBTTest();
		test.setType(IFOBTType.GOLD);
		ronde.setLaatsteIFOBTTest(test);

		try
		{
			test.setAfnameDatum(DateUtils.parseDate("01-05-2017", "dd-MM-yyyy"));
		}
		catch (ParseException e)
		{
		}
	}

	public static AfgeslotenMedewerkerOvereenkomst getDefaultFilledOvereenkomst()
	{
		AfgeslotenMedewerkerOvereenkomst afOvereenkomst = new AfgeslotenMedewerkerOvereenkomst();

		ZorgInstelling zorginstelling = new ZorgInstelling();
		zorginstelling.setActief(true);
		zorginstelling.setNaam(defaultValue);
		zorginstelling.setTelefoon("xxx-xxxxxxx");
		zorginstelling.setFax("xxx-xxxxxxx");

		Gebruiker gemachtigde = new Gebruiker();
		gemachtigde.setVoorletters(defaultValue);
		gemachtigde.setTussenvoegsel(defaultValue);
		gemachtigde.setAchternaam(defaultValue);
		gemachtigde.setAanhef(Aanhef.MEVR);
		zorginstelling.setGemachtigde(gemachtigde);

		Gebruiker zorgverlener = new Gebruiker();
		zorgverlener.setAchternaam(defaultValue);
		zorgverlener.setTussenvoegsel(defaultValue);
		zorgverlener.setVoorletters(defaultValue);
		zorgverlener.setAanhef(Aanhef.MEVR);
		zorgverlener.add(getDefaultFilledAdres());
		zorgverlener.setBignummer(defaultValue);

		zorginstelling.add(getDefaultFilledAdres());
		zorginstelling.add(getDefaultFilledAdres());
		zorgverlener.add(getDefaultFilledAdres());
		InstellingGebruiker instellingGebruiker = new InstellingGebruiker();
		instellingGebruiker.setMedewerker(zorgverlener);
		zorginstelling.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
		zorginstelling.getOrganisatieMedewerkers().add(instellingGebruiker);
		afOvereenkomst.setGebruiker(zorgverlener);

		try
		{
			afOvereenkomst.setStartDatum(DateUtils.parseDate("31-12-2000", "dd-MM-yyyy"));
			zorgverlener.setGeboortedatum(DateUtils.parseDate("31-12-2000", "dd-MM-yyyy"));
		}
		catch (ParseException e)
		{
			LOG.debug("Geboortedatum kon niet worden geparsed;");
		}

		return afOvereenkomst;
	}

	public static ColonIntakeAfspraak getDefaultFilledColonIntakeAfspraak()
	{
		ColoscopieCentrum il = new ColoscopieCentrum();
		il.setActief(true);

		AfspraakDefinitie afspraakDefinitie = new AfspraakDefinitie();
		afspraakDefinitie.setDuurAfspraakInMinuten(15);
		il.getAfspraakDefinities().add(afspraakDefinitie);

		il.add(getDefaultFilledAdres());
		il.add(getDefaultFilledAdres());

		Kamer locatie = new Kamer();
		locatie.setColoscopieCentrum(il);
		locatie.setName(defaultValue);
		locatie.setActief(true);

		ColonIntakeAfspraak afspraak = new ColonIntakeAfspraak();
		afspraak.setLocation(locatie);

		return afspraak;
	}

	private static BagAdres getDefaultFilledAdres()
	{
		BagAdres adres = new BagAdres();
		adres.setStraat("Teststraat");
		adres.setHuisnummer(28);
		adres.setPlaats("Teststad-Utrecht");
		adres.setHuisletter("B");
		adres.setPostcode("8888XX");
		adres.setGbaGemeente(new Gemeente());
		return adres;
	}

	static CervixUitnodiging getDefaultCervixUitnodiging()
	{
		GbaPersoon persoon = new GbaPersoon();
		persoon.setBsn(TestBsnGenerator.getValideBsn());

		Client client = new Client();
		client.setPersoon(persoon);

		CervixDossier dossier = new CervixDossier();
		dossier.setClient(client);

		CervixScreeningRonde ronde = new CervixScreeningRonde();
		dossier.getScreeningRondes().add(ronde);
		dossier.setLaatsteScreeningRonde(ronde);
		ronde.setDossier(dossier);

		CervixUitnodiging uitnodiging = new CervixUitnodiging();
		ronde.getUitnodigingen().add(uitnodiging);
		ronde.setLaatsteUitnodiging(uitnodiging);
		uitnodiging.setScreeningRonde(ronde);
		uitnodiging.setMonsterType(CervixMonsterType.UITSTRIJKJE);

		CervixUitstrijkje uitstrijkje = new CervixUitstrijkje();
		uitnodiging.setMonster(uitstrijkje);
		uitstrijkje.setMonsterId(StringUtils.leftPad("" + ThreadLocalRandom.current().nextLong(1L, 9999999999L), 10, '0'));
		uitstrijkje.setUitnodiging(uitnodiging);
		uitstrijkje.setControleLetters(CervixMonsterUtil.getMonsterControleLetters(uitstrijkje.getUitnodiging().getScreeningRonde().getDossier()));
		uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.BEOORDEELD_DOOR_CYTOLOGIE);

		return uitnodiging;
	}

	public static BMHKLaboratorium getDefaultBmhkLaboratorium()
	{
		BMHKLaboratorium lab = new BMHKLaboratorium();
		lab.setNaam("Testlaboratorium");
		lab.setActief(true);
		lab.setPatholoog("J.Dokter, medisch microbioloog");

		lab.add(getDefaultFilledAdres());
		lab.add(getDefaultFilledAdres());

		ZASRetouradres zasRetouradres = new ZASRetouradres();
		lab.getRetouradressen().add(zasRetouradres);
		BagAdres defaultFilledAdres = getDefaultFilledAdres();
		defaultFilledAdres.setHuisnummer(1000000);
		zasRetouradres.setAdres(defaultFilledAdres);
		return lab;
	}
}
