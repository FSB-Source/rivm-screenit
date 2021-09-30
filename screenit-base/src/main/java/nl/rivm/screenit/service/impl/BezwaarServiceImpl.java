package nl.rivm.screenit.service.impl;

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

import static nl.rivm.screenit.model.enums.BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK;
import static nl.rivm.screenit.model.enums.BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES;

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.cervix.CervixRondeDao;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.RedenOpnieuwAanvragenClientgegevens;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.Nationaliteit;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Polis;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.SUPPORTS)
public class BezwaarServiceImpl implements BezwaarService
{
	private static final Logger LOG = LoggerFactory.getLogger(BezwaarServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private LogService logService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private FileService fileService;

	@Autowired(required = false)
	private MammaBaseDossierService mammaBaseDossierService;

	@Autowired
	private BaseClientContactService baseClientContactService;

	@Autowired(required = false)
	private CervixBaseScreeningrondeService cervixBaseScreeningrondeService;

	@Autowired(required = false)
	private CervixMailService mailService;

	@Autowired(required = false)
	private CervixRondeDao rondeDao;

	@Autowired(required = false)
	private ColonDossierBaseService colonDossierBaseService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwaarAanvragen(Client client, BezwaarMoment bezwaarMoment)
	{

		client.getBezwaarMomenten().add(bezwaarMoment);
		DateTime nu = currentDateSupplier.getDateTime();

		LOG.info("Bezwaar: aanvraag");
		BezwaarBrief brief = briefService.maakBezwaarBrief(bezwaarMoment, BriefType.CLIENT_BEZWAAR_AANVRAAG, nu.toDate());
		bezwaarMoment.setClient(client);
		bezwaarMoment.getBrieven().add(brief);
		bezwaarMoment.setBezwaarAanvraag(brief);
		bezwaarMoment.setStatus(AanvraagBriefStatus.BRIEF);
		bezwaarMoment.setStatusDatum(nu.plusMillis(50).toDate());

		hibernateService.saveOrUpdate(bezwaarMoment);
		hibernateService.saveOrUpdate(client);

		LOG.info("Bezwaar: wacht op antwoord");
	}

	@Override
	public List<BezwaarGroupViewWrapper> getBezwaarGroupViewWrappers(BezwaarMoment moment, boolean verzoekTotBezwaarTeZien)
	{
		List<BezwaarGroupViewWrapper> lijstBezwaarViewWrappers = new ArrayList<BezwaarGroupViewWrapper>();
		List<Bezwaar> bezwaren = moment.getBezwaren();
		for (Bezwaar bezwaar : bezwaren)
		{
			if (!BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER.equals(bezwaar.getType()) || verzoekTotBezwaarTeZien)
			{
				Bevolkingsonderzoek onderzoek = bezwaar.getBevolkingsonderzoek();
				BezwaarGroupViewWrapper groupWrapper = getBezwaarGroupViewWrapperFromList(lijstBezwaarViewWrappers, onderzoek);
				BezwaarViewWrapper wrapper = getBezwaarViewWrapper(bezwaar.getType(), Boolean.TRUE);
				wrapper.setResourceKey("BezwaarType." + groupWrapper.getKey() + "." + wrapper.getType());
				groupWrapper.getBezwaren().add(wrapper);
				if (!lijstBezwaarViewWrappers.contains(groupWrapper))
				{
					lijstBezwaarViewWrappers.add(groupWrapper);
				}
			}
		}
		return lijstBezwaarViewWrappers;
	}

	public BezwaarGroupViewWrapper getBezwaarGroupViewWrapperFromList(List<BezwaarGroupViewWrapper> lijstBezwaarGroupViewWrappers, Bevolkingsonderzoek onderzoek)
	{
		String wrapperName = "ALGEMEEN";
		if (onderzoek != null)
		{
			wrapperName = onderzoek.toString();
		}
		for (BezwaarGroupViewWrapper groupWrapper : lijstBezwaarGroupViewWrappers)
		{
			if (wrapperName.equals(groupWrapper.getKey()))
			{
				return groupWrapper;
			}
		}

		return getGroupWrapper(onderzoek);
	}

	@Override
	public List<BezwaarGroupViewWrapper> getEditBezwaarGroupViewWrappers(Client client, BezwaarMoment laatstVoltooideMoment)
	{
		return getEditBezwaarGroupViewWrappers(client, laatstVoltooideMoment, true);
	}

	@Override
	public List<BezwaarGroupViewWrapper> getEditBezwaarGroupViewWrappers(Client client, BezwaarMoment laatstVoltooideMoment, boolean checkDossierBezwaar)
	{

		List<BezwaarGroupViewWrapper> lijstBezwaarViewWrappers = new ArrayList<BezwaarGroupViewWrapper>();
		lijstBezwaarViewWrappers.add(getGroupWrapper(null));

		List<Bevolkingsonderzoek> onderzoeken = doelgroepService.totWelkeBevolkingsonderzoekenHoortDezeClient(client);

		for (Bevolkingsonderzoek onderzoek : onderzoeken)
		{
			lijstBezwaarViewWrappers.add(getGroupWrapper(onderzoek));
		}

		for (BezwaarGroupViewWrapper groupWrapper : lijstBezwaarViewWrappers)
		{
			voegBezwaarToe(groupWrapper, laatstVoltooideMoment, checkDossierBezwaar);
		}
		return removeEmptyGroupViewWrappers(lijstBezwaarViewWrappers);
	}

	public List<BezwaarGroupViewWrapper> removeEmptyGroupViewWrappers(List<BezwaarGroupViewWrapper> wrappers)
	{
		List<BezwaarGroupViewWrapper> groupWrappersMetBezwaren = new ArrayList<BezwaarGroupViewWrapper>();
		for (BezwaarGroupViewWrapper wrapper : wrappers)
		{
			if (!wrapper.getBezwaren().isEmpty())
			{
				groupWrappersMetBezwaren.add(wrapper);
			}
		}
		return groupWrappersMetBezwaren;
	}

	private BezwaarGroupViewWrapper getGroupWrapper(Bevolkingsonderzoek onderzoek)
	{
		BezwaarGroupViewWrapper groupWrapper = new BezwaarGroupViewWrapper();
		if (onderzoek == null)
		{
			groupWrapper.setKey("ALGEMEEN");
		}
		else
		{
			groupWrapper.setBevolkingsonderzoek(onderzoek);
			groupWrapper.setKey(onderzoek.name());
		}
		return groupWrapper;
	}

	private void voegBezwaarToe(BezwaarGroupViewWrapper groupWrapper, BezwaarMoment laatstVoltooideMoment, boolean checkDossierBezwaar)
	{
		voegBezwaarToe(groupWrapper, laatstVoltooideMoment, false, checkDossierBezwaar);
	}

	private void voegBezwaarToe(BezwaarGroupViewWrapper groupWrapper, BezwaarMoment voltooideMoment, boolean clientPortaal, boolean checkDossierBezwaar)
	{
		Bevolkingsonderzoek onderzoek = groupWrapper.getBevolkingsonderzoek();
		for (BezwaarType type : BezwaarType.values())
		{
			List<Bevolkingsonderzoek> onderzoekenByType = Arrays.asList(type.getBevolkingsonderzoeken());
			if ((onderzoek == null && onderzoekenByType.isEmpty() || onderzoekenByType.contains(onderzoek))
				&& (!clientPortaal || !type.getOnzichtbaarOpClientPortaal())
				&& (!BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS.equals(type) || BezwaarUtil.isBezwaarActiefVoor(voltooideMoment, type, onderzoek, checkDossierBezwaar))
				&& !BezwaarType.GEEN_REGISTRATIE_GEBOORTELAND.equals(type))
			{
				BezwaarViewWrapper wrapper = getBezwaarViewWrapper(type, BezwaarUtil.isBezwaarActiefVoor(voltooideMoment, type, onderzoek, checkDossierBezwaar));
				wrapper.setResourceKey("BezwaarType." + groupWrapper.getKey() + "." + wrapper.getType());
				groupWrapper.getBezwaren().add(wrapper);
			}
		}
	}

	private BezwaarViewWrapper getBezwaarViewWrapper(BezwaarType type, Boolean actief)
	{
		BezwaarViewWrapper wrapper = new BezwaarViewWrapper();
		wrapper.setType(type);
		wrapper.setActief(actief);
		return wrapper;
	}

	@Override
	public void nogmaalsVersturenBezwaar(BezwaarMoment bezwaar)
	{

		LOG.info("Bezwaar: handtekening brief de deur uit;");
		DateTime nu = currentDateSupplier.getDateTime();
		BezwaarBrief brief = briefService.maakBezwaarBrief(bezwaar, BriefType.CLIENT_BEZWAAR_HANDTEKENING, nu.toDate());
		bezwaar.setStatus(AanvraagBriefStatus.BRIEF);
		bezwaar.getBrieven().add(brief);
		bezwaar.setStatusDatum(nu.plusMillis(50).toDate());
		bezwaar.setBezwaarAanvraag(brief);
		hibernateService.saveOrUpdate(bezwaar);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwaarAfronden(BezwaarMoment moment, Account account, List<BezwaarGroupViewWrapper> groupWrappers) throws IllegalStateException
	{
		List<Bezwaar> bezwaren = maakNieuweBezwaren(moment, groupWrappers);
		moment.setBezwaren(bezwaren);
		bezwaarAfronden(moment, account, true);
		BezwaarMoment resetMoment = maakResetMoment(moment, groupWrappers);
		if (resetMoment != null)
		{
			bezwaarAfronden(resetMoment, account, false);
		}
	}

	private BezwaarMoment maakResetMoment(BezwaarMoment moment, List<BezwaarGroupViewWrapper> groupWrappers)
	{
		BezwaarMoment resetMoment = null;
		boolean resetNodig = false;
		for (BezwaarGroupViewWrapper wrapper : groupWrappers)
		{
			for (BezwaarViewWrapper bezwaarView : wrapper.getBezwaren())
			{
				if (BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER.equals(bezwaarView.getType()) && bezwaarView.getActief())
				{
					bezwaarView.setActief(false);
					resetNodig = true;
				}
			}
		}
		if (resetNodig)
		{
			resetMoment = new BezwaarMoment();
			List<Bezwaar> bezwaren = maakNieuweBezwaren(resetMoment, groupWrappers);
			resetMoment.setBezwaren(bezwaren);
			resetMoment.setStatusDatum(moment.getStatusDatum());
			resetMoment.setStatus(moment.getStatus());
			resetMoment.setBezwaarBrief(moment.getBezwaarBrief());
			resetMoment.setClient(moment.getClient());
			resetMoment.setBezwaarDatum(moment.getBezwaarDatum());
			resetMoment.setBezwaarAanvraag(moment.getBezwaarAanvraag());
		}
		return resetMoment;
	}

	@Override
	public boolean isBezwaarNieuwVergelekeVorigeBezwaarMoment(BezwaarMoment nieuwBezwaarMoment, BezwaarType bezwaarType)
	{
		Client client = nieuwBezwaarMoment.getClient();
		int bezwarenGrote = client.getBezwaarMomenten().size();
		if (bezwarenGrote > 1)
		{
			return client.getBezwaarMomenten()
				.stream().sorted(Comparator.comparing(BezwaarMoment::getBezwaarDatum))
				.filter(bezwaarMoment -> !bezwaarMoment.getId().equals(nieuwBezwaarMoment.getId())
					&& AanvraagBriefStatus.VERWERKT.equals(bezwaarMoment.getStatus()))
				.reduce((first, second) -> second)
				.map(bezwaarMoment -> bezwaarMoment.getBezwaren()
					.stream()
					.noneMatch(bezwaar -> bezwaar.getType().equals(bezwaarType)))
				.orElse(true);
		}
		return true;
	}

	private void bezwaarAfronden(BezwaarMoment moment, Account account, boolean maakBrief)
	{
		Date nu = currentDateSupplier.getDate();
		Client client = moment.getClient();
		bezwarenOngedaanMaken(account, client.getLaatstVoltooideBezwaarMoment(), moment);

		moment.setStatus(AanvraagBriefStatus.VERWERKT);
		moment.setStatusDatum(nu);
		moment.setBezwaarDatum(nu);
		client.setLaatstVoltooideBezwaarMoment(moment);
		client.getBezwaarMomenten().add(moment);
		hibernateService.saveOrUpdateAll(moment, client);
		hibernateService.saveOrUpdateAll(moment.getBezwaren());

		if (maakBrief)
		{
			BezwaarBrief brief = briefService.maakBezwaarBrief(moment, BriefType.CLIENT_BEZWAAR_BEVESTIGING, nu);
			moment.setBevestigingsbrief(brief);
			moment.getBrieven().add(brief);
			hibernateService.saveOrUpdateAll(moment);
		}

		bezwaarAangepastLogging(account, moment);
		bezwarenDoorvoeren(moment);
	}

	private void bezwaarAangepastLogging(Account account, BezwaarMoment moment)
	{
		String loggingMelding = null;
		Map<BezwaarType, String> bezwarenMap = new HashMap<BezwaarType, String>();
		List<Bezwaar> bezwaren = moment.getBezwaren();
		if (CollectionUtils.isNotEmpty(bezwaren))
		{
			for (Bezwaar bezwaar : bezwaren)
			{
				String melding = null;
				if (!bezwarenMap.containsKey(bezwaar.getType()))
				{
					melding = bezwaar.getType().getNaam();
					if (bezwaar.getBevolkingsonderzoek() != null)
					{
						melding += "(" + bezwaar.getBevolkingsonderzoek().getAfkorting() + ")";
					}
					bezwarenMap.put(bezwaar.getType(), melding);
				}
				else
				{
					melding = bezwarenMap.get(bezwaar.getType());
					melding = melding.substring(0, melding.length() - 1);
					melding += ", " + bezwaar.getBevolkingsonderzoek().getAfkorting() + ")";
					bezwarenMap.put(bezwaar.getType(), melding);
				}
			}

			loggingMelding = "Volgende bezwaren zijn geldig: ";
			loggingMelding += String.join(", ", bezwarenMap.values());
		}
		else
		{
			loggingMelding = "Er zijn voor deze client geen bezwaren meer actief";
		}

		LogEvent logEvent = new LogEvent();
		logEvent.setMelding(loggingMelding);
		logEvent.setLevel(Level.INFO);

		logService.logGebeurtenis(LogGebeurtenis.CLIENT_BEZWAAR_AANGEPAST, logEvent, account, moment.getClient());
	}

	private List<Bezwaar> maakNieuweBezwaren(BezwaarMoment moment, List<BezwaarGroupViewWrapper> groupWrappers)
	{
		List<Bezwaar> bezwaren = new ArrayList<Bezwaar>();
		for (BezwaarGroupViewWrapper groupWrapper : groupWrappers)
		{
			for (BezwaarViewWrapper wrapper : groupWrapper.getBezwaren())
			{
				if (wrapper.getActief())
				{
					Bezwaar bezwaar = new Bezwaar();
					Bevolkingsonderzoek onderzoek = groupWrapper.getBevolkingsonderzoek();
					if (onderzoek != null)
					{
						bezwaar.setBevolkingsonderzoek(onderzoek);
					}
					bezwaar.setType(wrapper.getType());
					bezwaar.setBezwaarMoment(moment);
					bezwaren.add(bezwaar);
				}
			}
		}
		return bezwaren;
	}

	@Override
	public List<BezwaarGroupViewWrapper> getGroupWrapperForClientPortaal(BezwaarMoment laatstVoltooideMoment, Bevolkingsonderzoek bevolkingsonderzoek)
	{

		List<BezwaarGroupViewWrapper> lijstBezwaarViewWrappers = new ArrayList<BezwaarGroupViewWrapper>();
		lijstBezwaarViewWrappers.add(getGroupWrapper(null));
		lijstBezwaarViewWrappers.add(getGroupWrapper(bevolkingsonderzoek));

		for (BezwaarGroupViewWrapper groupWrapper : lijstBezwaarViewWrappers)
		{
			voegBezwaarToe(groupWrapper, laatstVoltooideMoment, true, true);
		}
		return lijstBezwaarViewWrappers;
	}

	private void bezwarenOngedaanMaken(Account account, BezwaarMoment huidigBezwaar, BezwaarMoment nieuwBezwaar)
	{
		if (nieuwBezwaar != null && huidigBezwaar != null)
		{
			for (Bezwaar bezwaar : huidigBezwaar.getBezwaren())
			{
				Client client = nieuwBezwaar.getClient();
				if (!BezwaarUtil.isBezwaarActiefVoor(nieuwBezwaar, bezwaar.getType(), bezwaar.getBevolkingsonderzoek(), true))
				{

					switch (bezwaar.getType())
					{
					case GEEN_OPNAME_UIT_BPR:
						clientService.vraagGbaGegevensOpnieuwAan(client, account, RedenOpnieuwAanvragenClientgegevens.BEZWAAR_INGETROKKEN);
						verwijderAdresGegevensVanClient(client);
						break;
					default:

					}
				}
			}
		}
	}

	private void verwijderAdresGegevensVanClient(Client client)
	{
		BagAdres adres = client.getPersoon().getGbaAdres();
		adres.setPlaats(null);
		adres.setPostcode(null);
		adres.setStraat(null);
		adres.setHuisnummer(null);
		adres.setHuisletter(null);
		adres.setHuisnummerAanduiding(null);
		adres.setHuisnummerToevoeging(null);
		adres.setLand(null);
		adres.setSoort(null);
		hibernateService.saveOrUpdateAll(client, adres);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwarenDoorvoeren(BezwaarMoment moment)
	{
		if (moment != null)
		{
			for (Bezwaar bezwaar : moment.getBezwaren())
			{
				switch (bezwaar.getType())
				{
				case GEEN_WETENSCHAPPELIJK_ONDERZOEK:
				case GEEN_KWALITEITSWAARBORGING:
					bezwaarWetenSchappelijkOnderzoekEnKwaliteitswaarborging(moment);
					break;
				case GEEN_OPNAME_UIT_BPR:
					bezwaarOpnameUitBPR(moment);
					break;
				case VERZOEK_TOT_VERWIJDERING_DOSSIER:
					bezwaarVerzoekTotVerwijderingDossier(bezwaar);
					break;
				case GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK:
					if (isBezwaarNieuwVergelekeVorigeBezwaarMoment(moment, GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK))
					{
						verwerkBezwaarLichaamsmateriaal(moment.getClient());
					}
					break;
				case GEEN_SIGNALERING_VERWIJSADVIES:
					if (isBezwaarNieuwVergelekeVorigeBezwaarMoment(moment, GEEN_SIGNALERING_VERWIJSADVIES))
					{
						verwerkGeenControleVerwijsAdvies(moment.getClient());
					}
					break;
				case GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS:
					verstuurXdsConsent(moment.getClient());
					break;
				default:

				}
			}
		}

		Client client = moment.getClient();
		client.setLaatstVoltooideBezwaarMoment(moment);
	}

	private void verstuurXdsConsent(Client client)
	{
		client.getMammaDossier().setXdsStatus(XdsStatus.TE_VERZENDEN);
		hibernateService.saveOrUpdate(client.getMammaDossier());
	}

	private void bezwaarOpnameUitBPR(BezwaarMoment moment)
	{
		Client client = moment.getClient();
		GbaPersoon persoon = client.getPersoon();

		persoon.setAanduidingBijzonderNederlanderschap(null);
		persoon.setAkteNummerOverlijden(null);
		persoon.setBurgelijkeStaat(null);
		persoon.setDatumAangaanPartnerschap(null);
		persoon.setDatumAanvangAdreshouding(null);
		persoon.setDatumOntbindingPartnerschap(null);
		persoon.setDatumVertrokkenUitNederland(null);
		persoon.setDatumVestigingNederland(null);
		persoon.setDocumentType(null);
		persoon.setEmailadres(null);
		persoon.setFaxnummer(null);
		persoon.setGbaGeboorteLand(null);
		persoon.setGbaNationaliteiten(new ArrayList<Nationaliteit>());
		persoon.setGeboorteplaats(null);
		persoon.setGeboorteland(null);
		persoon.setIndicatieGeheim(null);
		persoon.setMeerling(null);
		persoon.setMobielnummer(null);
		persoon.setOverlijdensdatum(null);
		persoon.setPolissen(new ArrayList<Polis>());
		persoon.setRedenOntbindingPartnerschap(null);
		persoon.setRegisterGemeenteAkteOverlijden(null);
		persoon.setTelefoonnummer1(null);
		persoon.setTelefoonnummer2(null);
		persoon.setTitel(null);
		persoon.setTitelCode(null);
		persoon.setWidControleDatum(null);
		persoon.setWidGecontroleerd(null);
		persoon.setWidGeregistreerdDoor(null);
		persoon.setWidnummer(null);
		persoon.setWidOrganisatieMedewerker(null);
		persoon.setWidRegistreerDatum(null);
		hibernateService.saveOrUpdate(persoon);

		client.setGbaStatus(GbaStatus.BEZWAAR);
		hibernateService.saveOrUpdate(client);
	}

	private void bezwaarWetenSchappelijkOnderzoekEnKwaliteitswaarborging(BezwaarMoment moment)
	{
		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(moment.getClient(), currentDateSupplier.getDate(), false);
		if (projectClient != null && Boolean.TRUE.equals(projectClient.getProject().getExcludeerBezwaar()))
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.BEZWAAR, null);
		}
	}

	private void bezwaarVerzoekTotVerwijderingDossier(Bezwaar bezwaar)
	{
		Client client = bezwaar.getBezwaarMoment().getClient();
		if (Bevolkingsonderzoek.COLON.equals(bezwaar.getBevolkingsonderzoek()))
		{
			ColonDossier dossier = client.getColonDossier();
			bezwaarVerzoekTotVerwijderingColonDossier(dossier);
		}

		if (Bevolkingsonderzoek.CERVIX.equals(bezwaar.getBevolkingsonderzoek()))
		{
			CervixDossier dossier = client.getCervixDossier();
			bezwaarVerzoekTotVerwijderingCervixDossier(dossier);
		}

		if (Bevolkingsonderzoek.MAMMA.equals(bezwaar.getBevolkingsonderzoek()))
		{
			mammaBaseDossierService.verwijderMammaDossier(client);
			verwijderNietLaatsteDefinitieveAfmeldingen(client.getMammaDossier());
		}
	}

	private void bezwaarVerzoekTotVerwijderingCervixDossier(CervixDossier dossier)
	{
		Client client = dossier.getClient();

		cervixBaseScreeningrondeService.verwijderCervixScreeningRondes(dossier);

		verwijderNietLaatsteDefinitieveAfmeldingen(dossier);

		baseClientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.CERVIX);

		if (dossier.getCisHistorie() != null)
		{
			CervixCISHistorie cervixCISHistorie = dossier.getCisHistorie();
			hibernateService.deleteAll(cervixCISHistorie.getCisHistorieRegels());
			dossier.setCisHistorie(null);

			hibernateService.delete(cervixCISHistorie);
		}

		dossier.setInactiefVanaf(null);
		dossier.setInactiefTotMet(null);
		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
		}
		hibernateService.saveOrUpdate(dossier);

		hibernateService.saveOrUpdate(client);

		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
		if (projectClient != null)
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, null);
		}
	}

	private void bezwaarVerzoekTotVerwijderingColonDossier(ColonDossier dossier)
	{
		Client client = dossier.getClient();

		if (dossier.getLaatsteScreeningRonde() != null)
		{
			colonDossierBaseService.setDatumVolgendeUitnodiging(dossier, ColonUitnodigingsintervalType.VERWIJDERD_DOSSIER);
		}

		List<ColonScreeningRonde> rondes = dossier.getScreeningRondes();
		dossier.setLaatsteScreeningRonde(null);
		dossier.setScreeningRondes(new ArrayList<ColonScreeningRonde>());
		if (CollectionUtils.isNotEmpty(rondes))
		{
			for (ColonScreeningRonde ronde : rondes)
			{
				List<ColonVerslag> verslagen = ronde.getVerslagen();
				if (CollectionUtils.isNotEmpty(verslagen))
				{
					hibernateService.deleteAll(verslagen);
				}
				if (ronde.getOpenUitnodiging() != null)
				{
					hibernateService.delete(ronde.getOpenUitnodiging());
				}
				hibernateService.delete(ronde);
			}
		}

		verwijderNietLaatsteDefinitieveAfmeldingen(dossier);

		baseClientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.COLON);

		if (CollectionUtils.isNotEmpty(client.getAfspraken()))
		{
			hibernateService.deleteAll(client.getAfspraken());
			client.setAfspraken(new ArrayList<Afspraak>());
		}

		if (!CollectionUtils.isEmpty(client.getComplicaties()))
		{
			for (Complicatie complicatie : client.getComplicaties())
			{
				hibernateService.delete(complicatie);
			}
		}

		if (dossier.getColonVooraankondiging() != null)
		{
			ColonVooraankondiging vooraankondigiging = dossier.getColonVooraankondiging();
			dossier.setColonVooraankondiging(null);
			hibernateService.delete(vooraankondigiging);
		}

		dossier.setInactiveerReden(null);
		dossier.setInactiefVanaf(null);
		dossier.setInactiefTotMet(null);

		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
		}

		hibernateService.saveOrUpdate(dossier);

		hibernateService.saveOrUpdate(client);

		ProjectClient projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
		if (projectClient != null)
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, null);
		}
	}

	private <D extends Dossier<?, A>, A extends Afmelding<?, ?, ?>> void verwijderNietLaatsteDefinitieveAfmeldingen(D dossier)
	{

		for (A afmelding : dossier.getAfmeldingen())
		{

			if (!afmelding.equals(dossier.getLaatsteAfmelding()) && afmelding.getHeraanmeldDatum() != null)
			{
				colonSpecifiekeVerwijderVerwerking(afmelding);
				if (afmelding.getHandtekeningDocumentAfmelding() != null)
				{
					fileService.delete(afmelding.getHandtekeningDocumentAfmelding(), true);
				}
				if (afmelding.getHandtekeningDocumentHeraanmelding() != null)
				{
					fileService.delete(afmelding.getHandtekeningDocumentHeraanmelding(), true);
				}
				hibernateService.delete(afmelding);
			}
		}
	}

	private <A extends Afmelding<?, ?, ?>> void colonSpecifiekeVerwijderVerwerking(A afmelding)
	{
		afmelding = (A) HibernateHelper.deproxy(afmelding);
		if (afmelding instanceof ColonAfmelding)
		{
			ColonAfmelding colonAfmelding = (ColonAfmelding) afmelding;

			if (ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(colonAfmelding.getReden()))
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
	}

	@Override
	public void nogmaalsVersturen(BezwaarMoment moment, Account account)
	{

		LOG.info("Bezwaar: " + BriefType.CLIENT_BEZWAAR_HANDTEKENING.toString() + "nogmaals verstuurd!");
		DateTime nu = currentDateSupplier.getDateTime();

		BezwaarBrief brief = briefService.maakBezwaarBrief(moment, BriefType.CLIENT_BEZWAAR_HANDTEKENING, nu.toDate());
		moment.setStatus(AanvraagBriefStatus.BRIEF);
		moment.getBrieven().add(brief);
		moment.setStatusDatum(nu.plusMillis(50).toDate());
		moment.setBezwaarAanvraag(brief);
		hibernateService.saveOrUpdate(moment);
	}

	@Override
	public void algemeneBezwaarBriefDoorvoeren(BezwaarBrief bezwaarBrief, Account account)
	{
		bezwaarBrief.setTegenhouden(false);
		hibernateService.saveOrUpdate(bezwaarBrief);
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_DOORVOEREN, account, bezwaarBrief.getClient(), bezwaarBrief.getBriefType() + ", was tegengehouden en wordt nu doorgevoerd.",
			bezwaarBrief.getBriefType().getOnderzoeken());

	}

	@Override
	public void algemeneBezwaarBriefTegenhouden(BezwaarBrief bezwaarBrief, Account account)
	{
		bezwaarBrief.setTegenhouden(true);
		hibernateService.saveOrUpdate(bezwaarBrief);
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_TEGENHOUDEN, account, bezwaarBrief.getClient(), BriefUtil.getBriefTypeNaam(bezwaarBrief) + ", wordt tegengehouden.",
			bezwaarBrief.getBriefType().getOnderzoeken());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwaarBRPIntrekken(Account account, Client client, UploadDocument document) throws IOException, IllegalStateException
	{
		try
		{
			fileService.saveOrUpdateUploadDocument(document, FileStoreLocation.BEZWAAR, client.getId());

			BezwaarMoment huidigeMoment = client.getLaatstVoltooideBezwaarMoment();

			BezwaarMoment nieuweBezwaarMoment = new BezwaarMoment();
			nieuweBezwaarMoment.setClient(client);
			nieuweBezwaarMoment.setBezwaarBrief(document);
			nieuweBezwaarMoment.setManier(ClientContactManier.DIRECT);
			for (Bezwaar bezwaar : huidigeMoment.getBezwaren())
			{
				if (BezwaarType.GEEN_OPNAME_UIT_BPR != bezwaar.getType())
				{
					nieuweBezwaarMoment.getBezwaren().add(bezwaar);
				}
			}

			bezwaarAfronden(nieuweBezwaarMoment, account, true);

			logService.logGebeurtenis(LogGebeurtenis.CLIENT_BEZWAAR_BRP_INGETROKKEN, account, client);

		}
		catch (IOException | IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden bij het intrekken van het Bezwaar BRP.", e);
			throw e;
		}
	}

	@Override
	public boolean bezwaarDocumentenVervangen(UploadDocument nieuwDocument, BezwaarMoment bezwaarMoment, UploadDocument huidigDocument, Account account)
	{
		bezwaarMoment.setBezwaarBrief(null);
		fileService.delete(huidigDocument, true);

		bezwaarMoment.setBezwaarBrief(nieuwDocument);
		try
		{
			fileService.saveOrUpdateUploadDocument(nieuwDocument, FileStoreLocation.BEZWAAR, bezwaarMoment.getClient().getId());
		}
		catch (IOException e)
		{
			LOG.error("Fout bij uploaden van een bezwaar formulier: ", e);
			return false;
		}

		logService.logGebeurtenis(LogGebeurtenis.VERVANGEN_DOCUMENT, account, bezwaarMoment.getClient(), bezwaarMoment.getBevestigingsbrief().getBriefType() + ", is vervangen.",
			bezwaarMoment.getBevestigingsbrief().getBevolkingsonderzoek());
		return true;
	}

	@Override
	@Transactional(propagation = Propagation.NEVER, readOnly = true)
	public boolean bezwarenGewijzigd(BezwaarMoment laatsteVoltooideBezwaarMoment, List<BezwaarGroupViewWrapper> wrappers, Bevolkingsonderzoek bvo)
	{
		List<String> nieuweBezwaarTypen = wrappers.stream()
			.filter(b -> bvo == null ? true : b.getBevolkingsonderzoek() == null || b.getBevolkingsonderzoek().equals(bvo))
			.flatMap(bm -> bm.getBezwaren().stream())
			.filter(b -> Boolean.TRUE.equals(b.getActief()))
			.map(bmm -> bvo + "_" + bmm.getType())
			.collect(Collectors.toList());

		if (laatsteVoltooideBezwaarMoment != null)
		{
			List<String> huidigeBezwaarTypen = laatsteVoltooideBezwaarMoment.getBezwaren().stream()
				.filter(b -> bvo == null ? true : b.getBevolkingsonderzoek() == null || b.getBevolkingsonderzoek().equals(bvo))
				.map(b -> b.getBevolkingsonderzoek() + "_" + b.getType())
				.collect(Collectors.toList());
			return !CollectionUtils.isEqualCollection(huidigeBezwaarTypen, nieuweBezwaarTypen);
		}
		return !nieuweBezwaarTypen.isEmpty();
	}

	private void verwerkGeenControleVerwijsAdvies(Client client)
	{
		if (mailService != null && rondeDao != null)
		{
			if (client.getCervixDossier().getLaatsteScreeningRonde() != null)
			{
				CervixScreeningRonde laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
				rondeDao.getOntvangenMonsters(laatsteScreeningRonde).stream()
					.filter(cervixMonster -> cervixMonster.getBrief() != null)
					.forEach(cervixMonster -> mailService.sendBMHKBezwaarControlleVerwijsAdviesMail(cervixMonster));
			}
		}
	}

	private void verwerkBezwaarLichaamsmateriaal(Client client)
	{
		if (mailService != null && rondeDao != null)
		{
			if (client.getCervixDossier().getLaatsteScreeningRonde() != null)
			{
				CervixScreeningRonde laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
				rondeDao.getOntvangenMonsters(laatsteScreeningRonde).stream()
					.filter(cervixMonster -> cervixMonster.getBrief() != null)
					.forEach(cervixMonster -> mailService.sendBMHKBezwaarLichaamsmateriaalMailAsync(cervixMonster));
			}
		}
	}

	@Override
	public boolean checkBezwaarInLaatsteBezwaarMomentAanwezigIs(Client client, BezwaarType bezwaarType)
	{
		BezwaarMoment laatstVoltooideBezwaarMoment = client.getLaatstVoltooideBezwaarMoment();
		if (laatstVoltooideBezwaarMoment == null)
		{
			return false;
		}
		return laatstVoltooideBezwaarMoment
			.getBezwaren().stream()
			.anyMatch(bezwaar -> bezwaar.getType().equals(bezwaarType));
	}

	@Override
	public boolean heeftBezwaarIngediendInAfgelopenAantalDagen(Client client, BezwaarType bezwaarType, Bevolkingsonderzoek bevolkingsonderzoek, int aantalDagen)
	{
		LocalDate bezwaarTermijn = currentDateSupplier.getLocalDate().minusDays(aantalDagen);
		for (BezwaarMoment bezwaarMoment : client.getBezwaarMomenten())
		{
			if (DateUtil.toLocalDate(bezwaarMoment.getBezwaarDatum()).isAfter(bezwaarTermijn))
			{
				for (Bezwaar bezwaar : bezwaarMoment.getBezwaren())
				{
					if (bezwaarType.equals(bezwaar.getType()) && bevolkingsonderzoek.equals(bezwaar.getBevolkingsonderzoek()))
					{
						return true;
					}
				}
			}
		}
		return false;
	}
}
