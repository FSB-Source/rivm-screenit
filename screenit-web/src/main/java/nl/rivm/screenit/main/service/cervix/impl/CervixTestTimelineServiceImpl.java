package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.algemeen.DeelnamemodusService;
import nl.rivm.screenit.main.service.cervix.CervixTestTimelineService;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DeelnamemodusDossierService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.TransgenderService;
import nl.rivm.screenit.service.cervix.CervixAanvraagService;
import nl.rivm.screenit.service.cervix.CervixTestTimelineTimeService;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixTestTimelineServiceImpl implements CervixTestTimelineService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private DossierService dossierService;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private ClientService clientService;

	@Autowired
	private TestService testService;

	@Autowired
	private CervixTestTimelineTimeService testTimelineTimeService;

	@Autowired
	private CervixAanvraagService aanvraagService;

	@Autowired
	private BriefHerdrukkenService briefHerdrukkenService;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private DeelnamemodusService deelnamemodusService;

	@Autowired
	private DeelnamemodusDossierService deelnamemodusDossierService;

	@Autowired
	private TransgenderService transgenderService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public List<TestTimelineRonde> getTimelineRondes(Client client)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<>();
		onderzoeken.add(Bevolkingsonderzoek.CERVIX);
		var rondes = dossierService.getScreeningRondeGebeurtenissen(client,
			new ClientDossierFilter(onderzoeken, false));

		return convertToTimeLineRondes(rondes);
	}

	private List<TestTimelineRonde> convertToTimeLineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<>();
		Integer index = 0;
		for (var ronde : rondeDossier)
		{
			TestTimelineRonde testTimeLineRonde;
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
		List<TestTimelineRonde> rondes = new ArrayList<>();
		for (var ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		List<TestVervolgKeuzeOptie> keuzes = new ArrayList<>();
		var dossier = client.getCervixDossier();
		if (dossier != null)
		{
			var ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null)
			{
				List<CervixUitnodiging> uitnodigingen = new ArrayList<>(ronde.getUitnodigingen());
				dossier.getScreeningRondes().forEach(sr -> sr.getUitnodigingen().stream()
					.filter(u -> (u.getMonster() != null && u.getMonster().getOntvangstScreeningRonde() != null && u.getMonster().getOntvangstScreeningRonde() == ronde)
						|| (u.getMonster() != null && magBeoordeeldDoorCytologie(u) && ronde.getUitstrijkjeCytologieUitslag() == null))
					.forEach(uitnodigingen::add));

				var monsterOntvangen = uitnodigingen.stream().anyMatch(u -> u.getMonster() != null && u.getMonster().getOntvangstScreeningRonde() != null);

				if (uitnodigingen.isEmpty())
				{
					keuzes.add(TestVervolgKeuzeOptie.CERVIX_VERSTUUR_UITNODIGING);
					keuzes.add(TestVervolgKeuzeOptie.CERVIX_UITSTEL_ZWANGER);
				}

				for (var uitnodiging : uitnodigingen)
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
	public boolean magNieuweRondeStarten(CervixDossier dossier)
	{
		if (dossier.getLaatsteScreeningRonde() == null)
		{
			var vooraankondigingsPeriode = preferenceService.getInteger(PreferenceKey.CERVIX_VOORAANKONDIGINGS_PERIODE.name(), 30);
			return CervixLeeftijdcategorie.aantalDagenTotBereikenMinimaleLeeftijd(DateUtil.toLocalDate(dossier.getClient().getPersoon().getGeboortedatum()),
				dateSupplier.getLocalDate()) <= vooraankondigingsPeriode;
		}
		return true;
	}

	@Override
	public boolean magOntvangen(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			var uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
			Date verzenddatumUitnodiging = null;
			MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(uitstrijkje.getUitnodiging().getBrief());
			if (mergedBrieven != null)
			{
				verzenddatumUitnodiging = mergedBrieven.getPrintDatum();
			}

			return (uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.NIET_ONTVANGEN)
				|| uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR))
				&& verzenddatumUitnodiging != null && uitstrijkje.getBrief() == null;
		}
		else if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS)
		{
			var zas = (CervixZas) uitnodiging.getMonster();
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
		var ronde = uitnodiging.getScreeningRonde();
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			var uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
			return uitstrijkjeStatus
				.equals(
					CervixUitstrijkjeStatus.ONTVANGEN)
				&& uitstrijkje.getBrief() == null
				&& (ronde.getMonsterHpvUitslag() == null
				|| ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag().equals(CervixHpvBeoordelingWaarde.POSITIEF)
				&& ronde.getUitstrijkjeCytologieUitslag() == null
				|| ronde.getInVervolgonderzoekDatum() != null && ronde.getUitstrijkjeVervolgonderzoekUitslag() == null);
		}
		else if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS)
		{
			var zas = (CervixZas) uitnodiging.getMonster();
			CervixZasStatus zasStatus = null;
			if (zas != null)
			{
				zasStatus = zas.getZasStatus();
			}
			return CervixZasStatus.ONTVANGEN.equals(zasStatus) && zas.getBrief() == null && ronde.getMonsterHpvUitslag() == null;
		}
		return true;
	}

	@Override
	public boolean magGeanalyseerdOpHpv(CervixUitnodiging uitnodiging)
	{
		var ronde = uitnodiging.getScreeningRonde();
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			var uitstrijkjeStatus = uitstrijkje.getUitstrijkjeStatus();
			return (uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.ONTVANGEN)
				|| uitstrijkjeStatus.equals(CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1))
				&& uitstrijkje.getBrief() == null && ronde.getMonsterHpvUitslag() == null;
		}
		else if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS)
		{
			var zas = (CervixZas) uitnodiging.getMonster();
			CervixZasStatus zasStatus = null;
			if (zas != null)
			{
				zasStatus = zas.getZasStatus();
			}
			return (CervixZasStatus.ONTVANGEN.equals(zasStatus) || CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_1.equals(zasStatus)) && zas.getBrief() == null
				&& ronde.getMonsterHpvUitslag() == null;
		}
		return true;
	}

	@Override
	public boolean magBeoordeeldDoorCytologie(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			return uitstrijkje.getCytologieOrder() != null && uitstrijkje.getCytologieOrder().getStatus() == CervixCytologieOrderStatus.VERSTUURD
				&& uitstrijkje.getCytologieVerslag() == null
				&& (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.ONTVANGEN
				|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1
				|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2);
		}
		else
		{
			return uitnodiging.getMonsterType() != CervixMonsterType.ZAS;
		}
	}

	@Override
	public boolean magHuisartsGekoppeldWorden(CervixUitnodiging uitnodiging)
	{
		var isUitstrijkje = CervixMonsterType.UITSTRIJKJE == uitnodiging.getMonsterType();
		var isLabformulier = isUitstrijkje && ((CervixUitstrijkje) uitnodiging.getMonster()).getLabformulier() != null;
		return isUitstrijkje && isLabformulier
			&& ((CervixUitstrijkje) uitnodiging.getMonster()).getLabformulier().getHuisartsLocatie() == null;
	}

	@Override
	public boolean magLabformulierGescand(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			Date verzenddatumUitnodiging = null;
			MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(uitstrijkje.getUitnodiging().getBrief());
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
		}
		else
		{
			return uitnodiging.getMonsterType() != CervixMonsterType.ZAS;
		}
	}

	@Override
	public boolean magLabformulierGecontroleerd(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			CervixLabformulierStatus labformulierStatus = null;
			if (uitstrijkje.getLabformulier() != null)
			{
				labformulierStatus = uitstrijkje.getLabformulier().getStatus();
			}
			return labformulierStatus != null && uitstrijkje.getLabformulier().getHuisartsLocatie() != null
				&& (labformulierStatus == CervixLabformulierStatus.AFGEKEURD || labformulierStatus == CervixLabformulierStatus.GESCAND
				|| labformulierStatus == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE && uitstrijkje.getCytologieOrder() == null);
		}
		else
		{
			return uitnodiging.getMonsterType() != CervixMonsterType.ZAS;
		}
	}

	@Override
	public boolean magLabformulierGecontroleerdVoorCytologie(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			var ronde = uitstrijkje.getOntvangstScreeningRonde();
			CervixLabformulierStatus labformulierStatus = null;
			if (uitstrijkje.getLabformulier() != null)
			{
				labformulierStatus = uitstrijkje.getLabformulier().getStatus();
			}
			return labformulierStatus == CervixLabformulierStatus.GECONTROLEERD && ronde.getMonsterHpvUitslag() != null
				&& ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvBeoordelingWaarde.POSITIEF;
		}
		else
		{
			return uitnodiging.getMonsterType() != CervixMonsterType.ZAS;
		}
	}

	@Override
	public boolean magOrderVerstuurd(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			var uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
			return uitstrijkje.getLabformulier() != null
				&& uitstrijkje.getLabformulier().getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
				&& uitstrijkje.getCytologieOrder() == null
				&& (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.ONTVANGEN
				|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1
				|| uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2);
		}
		else
		{
			return uitnodiging.getMonsterType() != CervixMonsterType.ZAS;
		}
	}

	@Override
	public boolean magVervolgonderzoekBrief(CervixUitnodiging uitnodiging)
	{
		var ronde = uitnodiging.getScreeningRonde();
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			return ronde.getInVervolgonderzoekDatum() != null && ronde.getUitnodigingVervolgonderzoek() == null;
		}
		else
		{
			return uitnodiging.getMonsterType() != CervixMonsterType.ZAS;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (var bsn : model.getBsns())
		{
			clienten.add(maakOfVindClient(model, bsn));
		}
		return clienten;
	}

	private Client maakOfVindClient(TestTimelineModel model, String bsn)
	{
		var persoon = new GbaPersoon();
		persoon.setBsn(bsn);
		persoon.setGeslacht(model.getGeslacht());
		persoon.setGeboortedatum(model.getGeboortedatum());

		var adres = new BagAdres();
		adres.setGbaGemeente(model.getGemeente());
		persoon.setGbaAdres(adres);

		var client = testService.maakClient(persoon);
		if (client.getCervixDossier() == null)
		{
			dossierFactory.maakBmhkEnBkDossiers(client);
		}
		return client;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<Client> maakOfWijzigClienten(TestTimelineModel model)
	{
		List<Client> clienten = new ArrayList<>();
		for (var bsn : model.getBsns())
		{
			var client = maakOfVindClient(model, bsn);
			clienten.add(client);
			var gbaPersoon = client.getPersoon();
			gbaPersoon.setGeslacht(model.getGeslacht());
			gbaPersoon.setGeboortedatum(model.getGeboortedatum());
			transgenderService.bijwerkenDeelnamemodus(client);
			hibernateService.saveOrUpdateAll(gbaPersoon);
		}
		return clienten;
	}

	@Override
	public List<String> validateTestClienten(List<Client> clienten)
	{
		Client eersteClient = null;
		var aantalUitnodigingen = 0;
		var aantalZasSnelkeuzeopties = 0;
		var eenLaatsteRondeBestaat = false;
		List<String> errorMeldingen = new ArrayList<>();
		for (var client : clienten)
		{
			if (eersteClient == null)
			{
				eersteClient = client;
				if (client.getCervixDossier() != null)
				{
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

			}
			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				var error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn()
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
				var zasSnelkeuzeOptiesError = "Het aantal ZAS snelkeuzeopties voor client-bsn: " + client.getPersoon().getBsn()
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
		var aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			var screeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && !screeningRonde.getUitnodigingen().isEmpty())
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
		var brief = ronde.getLaatsteBrief();

		var mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(BriefType.CERVIX_UITNODIGING);
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		mergedBrieven.setScreeningOrganisatie(ronde.getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		brief.setGegenereerd(true);
		brief.setMergedBrieven(mergedBrieven);
		var fakeMergeDocument = new UploadDocument();
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
		var laatsteZasUitnodiging = client.getCervixDossier().getLaatsteScreeningRonde().getLaatsteZasUitnodiging();
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
		var laatsteUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, false);
		if (laatsteUitnodiging != null)
		{
			return !baseBriefService.briefTypeWachtOpKlaarzettenInDezeRonde(laatsteUitnodiging.getBrief());
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void herdruk(CervixScreeningRonde ronde, Account account)
	{
		testTimelineTimeService.rekenDossierTerug(ronde.getDossier(), CervixTestTimeLineDossierTijdstip.HERDRUK);

		var laatsteAfgedrukteUitstrijkjeUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(ronde, false);
		var brief = laatsteAfgedrukteUitstrijkjeUitnodiging.getBrief();
		briefHerdrukkenService.opnieuwAanmaken(brief, account);
		verzendLaatsteBrief(ronde);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void registreerDeelnamewens(Client client)
	{
		var dto = deelnamemodusService.getDeelnamewensDto(client);
		dto.setDeelnamewensBmhk(true);
		deelnamemodusDossierService.pasDeelnamewensToe(client, dto, null);

	}
}
