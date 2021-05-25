package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.TestTimelineService;
import nl.rivm.screenit.main.service.cervix.CervixTestTimelineService;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.cervix.CervixAanvraagService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixTestTimelineTimeService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class CervixTestTimelineServiceImpl implements CervixTestTimelineService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixTestTimelineServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private CervixVervolgService vervolgService;

	@Autowired
	private DossierService dossierService;

	@Autowired
	private CervixBaseScreeningrondeService screeningrondeService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private TestService testService;

	@Autowired
	private CervixTestTimelineTimeService testTimelineTimeService;

	@Autowired
	private CervixMonsterDao monsterDao;

	@Autowired
	private CervixDossierDao dossierDao;

	@Autowired
	private CervixBepaalVervolgDao bepaalVervolgDao;

	@Autowired
	private CervixAanvraagService aanvraagService;

	@Autowired
	private BriefHerdrukkenService briefHerdrukkenService;

	@Autowired
	private BaseBriefService baseBriefService;

	@Override
	public List<TestTimelineRonde> getTimelineRondes(Client client)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		onderzoeken.add(Bevolkingsonderzoek.CERVIX);
		List<ScreeningRondeGebeurtenissen> rondes = dossierService.getScreeningRondeGebeurtenissen(client,
			new ClientDossierFilter(onderzoeken, false));

		List<TestTimelineRonde> timeLineRondes = convertToTimeLineRondes(rondes);
		return timeLineRondes;
	}

	private List<TestTimelineRonde> convertToTimeLineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<Integer, TestTimelineRonde>();
		Integer index = 0;
		for (ScreeningRondeGebeurtenissen ronde : rondeDossier)
		{
			TestTimelineRonde testTimeLineRonde = null;
			if (!rondes.containsKey(index))
			{
				testTimeLineRonde = new TestTimelineRonde();
				testTimeLineRonde.setRondeNummer(ronde.getRondenr());
				rondes.put(index, testTimeLineRonde);
			}
			else
			{
				testTimeLineRonde = rondes.get(index);
			}
			testTimeLineRonde.setCervixScreeningRondeDossier(ronde);
			index++;
		}
		return convertHashMapToList(rondes);
	}

	private List<TestTimelineRonde> convertHashMapToList(Map<Integer, TestTimelineRonde> map)
	{
		List<TestTimelineRonde> rondes = new ArrayList<TestTimelineRonde>();
		for (Map.Entry<Integer, TestTimelineRonde> ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();

		if (client.getPersoon().getGeslacht() == Geslacht.VROUW)
		{
			CervixDossier dossier = client.getCervixDossier();
			CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null)
			{
				List<CervixUitnodiging> uitnodigingen = new ArrayList<>(ronde.getUitnodigingen());
				dossier.getScreeningRondes().forEach(sr -> sr.getUitnodigingen().stream()
					.filter(u -> (u.getMonster() != null && u.getMonster().getOntvangstScreeningRonde() != null && u.getMonster().getOntvangstScreeningRonde() == ronde)
						|| (u.getMonster() != null && magBeoordeeldDoorCytologie(u) && ronde.getUitstrijkjeCytologieUitslag() == null))
					.forEach(uitnodigingen::add));

				boolean monsterOntvangen = uitnodigingen.stream().anyMatch(u -> u.getMonster() != null && u.getMonster().getOntvangstScreeningRonde() != null);

				for (CervixUitnodiging uitnodiging : uitnodigingen)
				{
					if (!monsterOntvangen && magAanvraag(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_AANVRAAG))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_AANVRAAG);
					}
					if (!monsterOntvangen && magOntvangen(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_ONTVANGEN))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_ONTVANGEN);
					}
					if (magNietAnalyseerbaar(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_NIET_ANALYSEERBAAR))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_NIET_ANALYSEERBAAR);
					}
					if (magGeanalyseerdOpHpv(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_GEANALYSEERD_OP_HPV))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_GEANALYSEERD_OP_HPV);
					}
					if (magBeoordeeldDoorCytologie(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_BEOORDEELD_DOOR_CYTOLOGIE))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_BEOORDEELD_DOOR_CYTOLOGIE);
					}
					if (magLabformulierGescand(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_LABFORMULIER_GESCAND))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_LABFORMULIER_GESCAND);
					}
					if (magHuisartsGekoppeldWorden(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_HUISARTS_KOPPELEN))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_HUISARTS_KOPPELEN);
					}
					if (magLabformulierGecontroleerd(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_LABFORMULIER_GECONTROLEERD))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_LABFORMULIER_GECONTROLEERD);
					}
					if (magLabformulierGecontroleerdVoorCytologie(uitnodiging)
						&& !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_LABFORMULIER_GECONTROLEERD_VOOR_CYTOLOGIE))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_LABFORMULIER_GECONTROLEERD_VOOR_CYTOLOGIE);
					}
					if (magOrderVerstuurd(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_ORDER_VERSTUURD))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_ORDER_VERSTUURD);
					}
					if (magVervolgonderzoekBrief(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_VERVOLGONDERZOEK_BRIEF))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_VERVOLGONDERZOEK_BRIEF);
					}

					if (magZASActiesUitvoeren(uitnodiging) && !keuzes.contains(TestVervolgKeuzeOptie.CERVIX_ZAS))
					{
						keuzes.add(TestVervolgKeuzeOptie.CERVIX_ZAS);
					}
				}

				if (magHerdruk(ronde))
				{
					keuzes.add(TestVervolgKeuzeOptie.CERVIX_HERDRUK);
				}
			}
		}
		return keuzes;
	}

	@Override
	public boolean magOntvangen(CervixUitnodiging uitnodiging)
	{
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:

			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixUitstrijkjeStatus uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
			Date verzenddatumUitnodiging = null;
			CervixMergedBrieven mergedBrieven = uitstrijkje.getUitnodiging().getBrief().getMergedBrieven();
			if (mergedBrieven != null)
			{
				verzenddatumUitnodiging = mergedBrieven.getPrintDatum();
			}

			return (uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.NIET_ONTVANGEN)
				|| uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR))
				&& verzenddatumUitnodiging != null && uitstrijkje.getBrief() == null;

		case ZAS:
			CervixZas zas = (CervixZas) uitnodiging.getMonster();
			CervixZasStatus zasStatus = null;
			if (zas != null)
			{
				zasStatus = zas.getZasStatus();
			}
			return CervixZasStatus.VERSTUURD.equals(zasStatus) || CervixZasStatus.NIET_ANALYSEERBAAR.equals(zasStatus);
		}
		return true;
	}

	@Override
	public boolean magAanvraag(CervixUitnodiging uitnodiging)
	{
		return uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE && aanvraagService.magDigitaalLabformulierAanvragen((CervixUitstrijkje) uitnodiging.getMonster());
	}

	@Override
	public boolean magNietAnalyseerbaar(CervixUitnodiging uitnodiging)
	{
		CervixScreeningRonde ronde = uitnodiging.getScreeningRonde();
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixUitstrijkjeStatus uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
			return uitstrijkjeStatus
				.equals(
					CervixUitstrijkjeStatus.ONTVANGEN)
				&& uitstrijkje.getBrief() == null
				&& (ronde.getMonsterHpvUitslag() == null
					|| ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag().equals(CervixHpvUitslag.POSITIEF)
						&& ronde.getUitstrijkjeCytologieUitslag() == null
					|| ronde.getInVervolgonderzoekDatum() != null && ronde.getUitstrijkjeVervolgonderzoekUitslag() == null);

		case ZAS:
			CervixZas zas = (CervixZas) uitnodiging.getMonster();
			CervixZasStatus zasStatus = null;
			if (zas != null)
			{
				zasStatus = zas.getZasStatus();
			}
			return CervixZasStatus.ONTVANGEN.equals(zasStatus) && zas != null && zas.getBrief() == null && ronde.getMonsterHpvUitslag() == null;
		}
		return true;
	}

	@Override
	public boolean magGeanalyseerdOpHpv(CervixUitnodiging uitnodiging)
	{
		CervixScreeningRonde ronde = uitnodiging.getScreeningRonde();
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixUitstrijkjeStatus uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
			return (uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.ONTVANGEN)
				|| uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1))
				&& uitstrijkje.getBrief() == null && ronde.getMonsterHpvUitslag() == null;
		case ZAS:
			CervixZas zas = (CervixZas) uitnodiging.getMonster();
			CervixZasStatus zasStatus = null;
			if (zas != null)
			{
				zasStatus = zas.getZasStatus();
			}
			return (CervixZasStatus.ONTVANGEN.equals(zasStatus) || CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_1.equals(zasStatus)) && zas != null
				&& zas.getBrief() == null
				&& ronde.getMonsterHpvUitslag() == null;
		}
		return true;
	}

	@Override
	public boolean magBeoordeeldDoorCytologie(CervixUitnodiging uitnodiging)
	{
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			return uitstrijkje.getCytologieOrder() != null && uitstrijkje.getCytologieOrder().getStatus() == CervixCytologieOrderStatus.VERSTUURD
				&& uitstrijkje.getCytologieVerslag() == null
				&& (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.ONTVANGEN
					|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1
					|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2);
		case ZAS:
			return false;
		}
		return true;
	}

	@Override
	public boolean magHuisartsGekoppeldWorden(CervixUitnodiging uitnodiging)
	{
		boolean isUitstrijkje = CervixMonsterType.UITSTRIJKJE == uitnodiging.getMonsterType();
		boolean isLabformulier = isUitstrijkje && ((CervixUitstrijkje) uitnodiging.getMonster()).getLabformulier() != null;
		boolean nogGeenHuisartsLocatie = isUitstrijkje && isLabformulier
			&& ((CervixUitstrijkje) uitnodiging.getMonster()).getLabformulier().getHuisartsLocatie() == null;
		return nogGeenHuisartsLocatie;
	}

	@Override
	public boolean magLabformulierGescand(CervixUitnodiging uitnodiging)
	{
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			Date verzenddatumUitnodiging = null;
			CervixMergedBrieven mergedBrieven = uitstrijkje.getUitnodiging().getBrief().getMergedBrieven();
			if (mergedBrieven != null)
			{
				verzenddatumUitnodiging = mergedBrieven.getPrintDatum();
			}
			CervixLabformulierStatus labformulierStatus = null;
			if (uitstrijkje.getLabformulier() != null)
			{
				labformulierStatus = uitstrijkje.getLabformulier().getStatus();
			}
			return verzenddatumUitnodiging != null && labformulierStatus == null || labformulierStatus == CervixLabformulierStatus.AFGEKEURD
				|| labformulierStatus == CervixLabformulierStatus.GECONTROLEERD;
		case ZAS:
			return false;
		}
		return true;
	}

	@Override
	public boolean magLabformulierGecontroleerd(CervixUitnodiging uitnodiging)
	{
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixLabformulierStatus labformulierStatus = null;
			if (uitstrijkje.getLabformulier() != null)
			{
				labformulierStatus = uitstrijkje.getLabformulier().getStatus();
			}
			return labformulierStatus != null && uitstrijkje.getLabformulier().getHuisartsLocatie() != null
				&& (labformulierStatus == CervixLabformulierStatus.AFGEKEURD || labformulierStatus == CervixLabformulierStatus.GESCAND
					|| labformulierStatus == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE && uitstrijkje.getCytologieOrder() == null);
		case ZAS:
			return false;
		}
		return true;
	}

	@Override
	public boolean magLabformulierGecontroleerdVoorCytologie(CervixUitnodiging uitnodiging)
	{
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixScreeningRonde ronde = uitstrijkje.getOntvangstScreeningRonde();
			CervixLabformulierStatus labformulierStatus = null;
			if (uitstrijkje.getLabformulier() != null)
			{
				labformulierStatus = uitstrijkje.getLabformulier().getStatus();
			}
			return labformulierStatus == CervixLabformulierStatus.GECONTROLEERD && ronde.getMonsterHpvUitslag() != null
				&& ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF;
		case ZAS:
			return false;
		}
		return true;
	}

	@Override
	public boolean magOrderVerstuurd(CervixUitnodiging uitnodiging)
	{
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			return uitstrijkje.getLabformulier() != null
				&& uitstrijkje.getLabformulier().getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
				&& uitstrijkje.getCytologieOrder() == null
				&& (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.ONTVANGEN
					|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1
					|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2);
		case ZAS:
			return false;
		}
		return true;
	}

	@Override
	public boolean magVervolgonderzoekBrief(CervixUitnodiging uitnodiging)
	{
		CervixScreeningRonde ronde = uitnodiging.getScreeningRonde();
		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			return ronde.getInVervolgonderzoekDatum() != null && ronde.getUitnodigingVervolgonderzoek() == null;
		case ZAS:
			return false;
		}
		return true;
	}

	@Override
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (String bsn : model.getBsns())
		{
			GbaPersoon persoon = new GbaPersoon();
			persoon.setBsn(bsn);
			persoon.setGeslacht(model.getGeslacht());
			persoon.setGeboortedatum(model.getGeboortedatum());

			BagAdres adres = new BagAdres();
			adres.setGbaGemeente(model.getGemeente());
			persoon.setGbaAdres(adres);
			clienten.add(testService.maakClient(persoon));
		}
		return clienten;
	}

	@Override
	public List<String> validateTestClienten(List<Client> clienten)
	{
		Client eersteClient = null;
		int aantalUitnodigingen = 0;
		int aantalZasSnelkeuzeopties = 0;
		boolean eenLaatsteRondeBestaat = false;
		List<String> errorMeldingen = new ArrayList<>();
		for (Client client : clienten)
		{
			if (eersteClient == null)
			{
				eersteClient = client;
				aantalUitnodigingen = getAantalUitnodigingen(client);
				if (client.getCervixDossier().getLaatsteScreeningRonde() != null)
				{
					aantalZasSnelkeuzeopties = getZasSnelKeuzeOpties(client).size();
				}
				if (client.getCervixDossier().getLaatsteScreeningRonde() != null)
				{
					eenLaatsteRondeBestaat = true;
				}

			}
			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				String error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn()
					+ " is niet gelijk aan de overige testcliënten.";
				addErrorMelding(error, errorMeldingen);
			}

			if (client.getCervixDossier().getLaatsteScreeningRonde() == null && eenLaatsteRondeBestaat)
			{
				addErrorMelding(
					"Laatste ronde ontbreekt voor client-bsn " + client.getPersoon().getBsn() + " en is ongelijk aan overige testcliënten",
					errorMeldingen);
			}
			if (client.getCervixDossier().getLaatsteScreeningRonde() != null
				&& aantalZasSnelkeuzeopties != getZasSnelKeuzeOpties(client).size())
			{
				String zasSnelkeuzeOptiesError = "Het aantal ZAS snelkeuzeopties voor client-bsn: " + client.getPersoon().getBsn()
					+ "is niet gelijk aan de overige testcliënten.";
				addErrorMelding(zasSnelkeuzeOptiesError, errorMeldingen);
			}
		}
		return errorMeldingen;
	}

	private void addErrorMelding(String error, List<String> meldingen)
	{
		LOG.error(error);
		meldingen.add(error);
	}

	private int getAantalUitnodigingen(Client client)
	{
		int aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			CervixScreeningRonde screeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && screeningRonde.getUitnodigingen().size() > 0)
			{
				aantalUitnodigingen = screeningRonde.getUitnodigingen().size();
			}
		}
		return aantalUitnodigingen;
	}

	private boolean heeftLaatsteScreeningsRonde(Client client)
	{
		return client.getCervixDossier().getLaatsteScreeningRonde() != null;
	}

	@Override
	public boolean magZASActiesUitvoeren(CervixUitnodiging uitnodiging)
	{
		return uitnodiging.getScreeningRonde().getMonsterHpvUitslag() == null;
	}

	private void verzendLaatsteBrief(CervixScreeningRonde ronde)
	{
		CervixBrief brief = ronde.getLaatsteBrief();

		CervixMergedBrieven mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(BriefType.CERVIX_UITNODIGING);
		mergedBrieven.setBrieven(new ArrayList<>());
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		mergedBrieven.setScreeningOrganisatie(ronde.getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		brief.setGegenereerd(true);
		brief.setMergedBrieven(mergedBrieven);
		UploadDocument fakeMergeDocument = new UploadDocument();
		fakeMergeDocument.setActief(true);
		fakeMergeDocument.setNaam("dummy_testservice_brief_niet_openen");
		hibernateService.saveOrUpdate(fakeMergeDocument);
		mergedBrieven.setMergedBrieven(fakeMergeDocument);
		hibernateService.saveOrUpdate(mergedBrieven);
		hibernateService.saveOrUpdate(brief);
	}

	@Override
	public List<CervixTestTimeLineDossierTijdstip> getZasSnelKeuzeOpties(Client client)
	{
		List<CervixTestTimeLineDossierTijdstip> mogelijkeActies = new ArrayList<>();
		CervixUitnodiging laatsteZasUitnodiging = client.getCervixDossier().getLaatsteScreeningRonde().getLaatsteZasUitnodiging();
		if (laatsteZasUitnodiging == null || laatsteZasUitnodiging.getMonster() != null)
		{
			mogelijkeActies.add(CervixTestTimeLineDossierTijdstip.ZAS_AANVRAAG);
			mogelijkeActies.add(CervixTestTimeLineDossierTijdstip.ZAS_KLAARGEZET);
			mogelijkeActies.add(CervixTestTimeLineDossierTijdstip.ZAS_SAMENGESTELD);
		}
		if (laatsteZasUitnodiging != null)
		{
			if (!laatsteZasUitnodiging.isVerstuurd())
			{
				mogelijkeActies.add(CervixTestTimeLineDossierTijdstip.ZAS_KLAARGEZET);
				mogelijkeActies.add(CervixTestTimeLineDossierTijdstip.ZAS_SAMENGESTELD);
			}
			else
			{
				mogelijkeActies.add(CervixTestTimeLineDossierTijdstip.ZAS_SAMENGESTELD);
			}
		}
		return mogelijkeActies;
	}

	@Override
	public boolean magHerdruk(CervixScreeningRonde ronde)
	{
		CervixUitnodiging laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, false);
		if (laatsteUitnodiging != null)
		{
			return !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(laatsteUitnodiging.getBrief());
		}
		return false;
	}

	@Override
	public void herdruk(CervixScreeningRonde ronde, Account account)
	{
		testTimelineTimeService.rekenDossierTerug(ronde.getDossier(), CervixTestTimeLineDossierTijdstip.HERDRUK);

		CervixUitnodiging laatsteAfgedrukteUitstrijkjeUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, false);
		CervixBrief brief = laatsteAfgedrukteUitstrijkjeUitnodiging.getBrief();
		briefHerdrukkenService.opnieuwAanmaken(brief, account);
		verzendLaatsteBrief(ronde);
	}
}
