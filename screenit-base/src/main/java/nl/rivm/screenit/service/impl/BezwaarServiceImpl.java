package nl.rivm.screenit.service.impl;

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

import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.comparator.BezwaarComparator;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.envers.RevisionKenmerkInThreadHolder;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.cervix.CervixBaseMonsterRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixBaseDossierService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.model.enums.BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK;
import static nl.rivm.screenit.model.enums.BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES;
import static nl.topicuszorg.util.collections.CollectionUtils.isEqualCollection;
import static nl.topicuszorg.util.collections.CollectionUtils.isNotEmpty;

@Slf4j
@Component
@Transactional(propagation = Propagation.SUPPORTS)
public class BezwaarServiceImpl implements BezwaarService
{
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
	private UploadDocumentService uploadDocumentService;

	@Autowired(required = false)
	private MammaBaseDossierService mammaBaseDossierService;

	@Autowired(required = false)
	private CervixBaseDossierService cervixBaseDossierService;

	@Autowired(required = false)
	private CervixMailService mailService;

	@Autowired(required = false)
	private CervixBaseMonsterRepository monsterRepository;

	@Autowired(required = false)
	private ColonDossierBaseService colonDossierBaseService;

	@Autowired
	private ApplicationEventPublisher applicationEventPublisher;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private BaseGbaVraagService baseGbaVraagService;

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

	@Override
	public List<BezwaarGroupViewWrapper> getBezwaarGroupViewWrappers(BezwaarMoment moment, boolean verzoekTotBezwaarTeZien)
	{
		List<BezwaarGroupViewWrapper> lijstBezwaarViewWrappers = new ArrayList<>();
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

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwaarAanvragen(Client client, BezwaarMoment bezwaarMoment)
	{

		client.getBezwaarMomenten().add(bezwaarMoment);
		var nu = currentDateSupplier.getDate();

		LOG.info("Bezwaar: aanvraag");
		BezwaarBrief brief = briefService.maakBezwaarBrief(bezwaarMoment, BriefType.CLIENT_BEZWAAR_AANVRAAG, nu);
		bezwaarMoment.setClient(client);
		bezwaarMoment.getBrieven().add(brief);
		bezwaarMoment.setBezwaarAanvraag(brief);
		bezwaarMoment.setStatus(AanvraagBriefStatus.BRIEF);
		bezwaarMoment.setStatusDatum(DateUtil.plusTijdseenheid(nu, 50, ChronoUnit.MILLIS));

		hibernateService.saveOrUpdate(bezwaarMoment);
		hibernateService.saveOrUpdate(client);

		LOG.info("Bezwaar: wacht op antwoord");
	}

	@Override
	public void nogmaalsVersturenBezwaar(BezwaarMoment bezwaar)
	{

		LOG.info("Bezwaar: handtekening brief de deur uit;");
		var nu = currentDateSupplier.getDate();
		BezwaarBrief brief = briefService.maakBezwaarBrief(bezwaar, BriefType.CLIENT_BEZWAAR_HANDTEKENING, nu);
		bezwaar.setStatus(AanvraagBriefStatus.BRIEF);
		bezwaar.getBrieven().add(brief);
		bezwaar.setStatusDatum(DateUtil.plusTijdseenheid(nu, 50, ChronoUnit.MILLIS));
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

	@Override
	public boolean isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(BezwaarMoment nieuwBezwaarMoment, BezwaarType bezwaarType)
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

	@Override
	public void nogmaalsVersturen(BezwaarMoment moment, Account account)
	{

		LOG.info("Bezwaar: " + BriefType.CLIENT_BEZWAAR_HANDTEKENING.toString() + "nogmaals verstuurd!");
		var nu = currentDateSupplier.getDate();

		BezwaarBrief brief = briefService.maakBezwaarBrief(moment, BriefType.CLIENT_BEZWAAR_HANDTEKENING, nu);
		moment.setStatus(AanvraagBriefStatus.BRIEF);
		moment.getBrieven().add(brief);
		moment.setStatusDatum(DateUtil.plusTijdseenheid(nu, 50, ChronoUnit.MILLIS));
		moment.setBezwaarAanvraag(brief);
		hibernateService.saveOrUpdate(moment);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwarenDoorvoeren(BezwaarMoment moment)
	{
		var client = moment.getClient();
		for (Bezwaar bezwaar : moment.getBezwaren())
		{
			switch (bezwaar.getType())
			{
			case GEEN_WETENSCHAPPELIJK_ONDERZOEK:
			case GEEN_KWALITEITSWAARBORGING:
				bezwaarWetenSchappelijkOnderzoekEnKwaliteitswaarborging(moment);
				break;
			case GEEN_OPNAME_UIT_BPR:
				bezwaarOpnameUitBrp(client);
				break;
			case VERZOEK_TOT_VERWIJDERING_DOSSIER:
				bezwaarVerzoekTotVerwijderingDossier(bezwaar);
				break;
			case GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK:
				if (isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(moment, GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK))
				{
					verwerkBezwaarLichaamsmateriaal(client);
				}
				break;
			case GEEN_SIGNALERING_VERWIJSADVIES:
				if (isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(moment, GEEN_SIGNALERING_VERWIJSADVIES))
				{
					verwerkGeenControleVerwijsAdvies(client);
				}
				break;
			case GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS:
				verstuurXdsConsent(client);
				break;
			default:
			}
		}

		client.setLaatstVoltooideBezwaarMoment(moment);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwaarBRPIntrekken(Account account, Client client, UploadDocument document) throws IOException, IllegalStateException
	{
		try
		{
			uploadDocumentService.saveOrUpdate(document, FileStoreLocation.BEZWAAR, client.getId());

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
		uploadDocumentService.delete(huidigDocument);

		bezwaarMoment.setBezwaarBrief(nieuwDocument);
		try
		{
			uploadDocumentService.saveOrUpdate(nieuwDocument, FileStoreLocation.BEZWAAR, bezwaarMoment.getClient().getId());
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
			return !isEqualCollection(huidigeBezwaarTypen, nieuweBezwaarTypen);
		}
		return !nieuweBezwaarTypen.isEmpty();
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

	@Override
	public BezwaarBrief getNogNietVerwerkteBezwaarBrief(List<BezwaarMoment> unsorted)
	{
		List<BezwaarMoment> bezwaren = new ArrayList<>(unsorted);
		BezwaarBrief result = null;
		bezwaren.sort(new BezwaarComparator());

		if (!bezwaren.isEmpty() && !AanvraagBriefStatus.VERWERKT.equals(bezwaren.get(0).getStatus()))
		{
			result = bezwaren.get(0).getBezwaarAanvraag();
		}
		return result;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void bezwaarAfrondenVanuitClientPortaal(Client client, List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers)
	{
		BezwaarBrief nogNietVerwerkteBezwaarBrief = getNogNietVerwerkteBezwaarBrief(client.getBezwaarMomenten());
		if (nogNietVerwerkteBezwaarBrief != null)
		{
			baseBriefService.briefTegenhouden(nogNietVerwerkteBezwaarBrief, client);
		}

		bezwaarAfronden(haalBezwaarMomentOp(nogNietVerwerkteBezwaarBrief, client), client, bezwaarGroupViewWrappers);

	}

	private BezwaarMoment haalBezwaarMomentOp(BezwaarBrief bezwaarBrief, Client client)
	{
		if (bezwaarBrief != null)
		{
			return bezwaarBrief.getBezwaarMoment();
		}
		else
		{
			BezwaarMoment moment = new BezwaarMoment();
			moment.setClient(client);
			moment.setManier(ClientContactManier.DIRECT);
			return moment;
		}
	}

	public List<BezwaarGroupViewWrapper> removeEmptyGroupViewWrappers(List<BezwaarGroupViewWrapper> wrappers)
	{
		List<BezwaarGroupViewWrapper> groupWrappersMetBezwaren = new ArrayList<>();
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
		Map<BezwaarType, String> bezwarenMap = new HashMap<>();
		List<Bezwaar> bezwaren = moment.getBezwaren();
		if (isNotEmpty(bezwaren))
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

	private void bezwarenOngedaanMaken(Account account, BezwaarMoment huidigBezwaar, BezwaarMoment nieuwBezwaar)
	{
		if (nieuwBezwaar != null && huidigBezwaar != null)
		{
			for (Bezwaar bezwaar : huidigBezwaar.getBezwaren())
			{
				Client client = nieuwBezwaar.getClient();
				if (!BezwaarUtil.isBezwaarActiefVoor(nieuwBezwaar, bezwaar.getType(), bezwaar.getBevolkingsonderzoek(), true)
					&& bezwaar.getType() == BezwaarType.GEEN_OPNAME_UIT_BPR)
				{
					baseGbaVraagService.verzoekPlaatsIndicatieBijIntrekkenBezwaarBrp(client, account);
					verwijderAdresGegevensVanClient(client);
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

	private void verstuurXdsConsent(Client client)
	{
		client.getMammaDossier().setXdsStatus(XdsStatus.TE_VERZENDEN);
		hibernateService.saveOrUpdate(client.getMammaDossier());
	}

	private void bezwaarOpnameUitBrp(Client client)
	{
		wisPersoonsGegevensVoorMakenBezwaarBrp(client.getPersoon());

		client.setGbaStatus(GbaStatus.BEZWAAR);
		hibernateService.saveOrUpdate(client);

		baseGbaVraagService.verzoekVerwijderIndicatieBijBezwaarBrp(client);
	}

	private void wisPersoonsGegevensVoorMakenBezwaarBrp(GbaPersoon persoon)
	{

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
		persoon.setGbaNationaliteiten(new ArrayList<>());
		persoon.setGeboorteplaats(null);
		persoon.setGeboorteland(null);
		persoon.setIndicatieGeheim(null);
		persoon.setMeerling(null);
		persoon.setMobielnummer(null);
		persoon.setOverlijdensdatum(null);
		persoon.setPolissen(new ArrayList<>());
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
		RevisionKenmerkInThreadHolder.setKenmerk(RevisionKenmerk.DOSSIERVERWIJDERING_DOOR_BEZWAAR, applicationEventPublisher);
		if (Bevolkingsonderzoek.COLON.equals(bezwaar.getBevolkingsonderzoek()))
		{
			ColonDossier dossier = client.getColonDossier();
			colonDossierBaseService.maakDossierLeeg(dossier);
		}

		if (Bevolkingsonderzoek.CERVIX.equals(bezwaar.getBevolkingsonderzoek()))
		{
			CervixDossier dossier = client.getCervixDossier();
			cervixBaseDossierService.maakDossierLeeg(dossier);
		}

		if (Bevolkingsonderzoek.MAMMA.equals(bezwaar.getBevolkingsonderzoek()))
		{
			MammaDossier dossier = client.getMammaDossier();
			mammaBaseDossierService.maakDossierLeeg(dossier);
		}
	}

	private void verwerkGeenControleVerwijsAdvies(Client client)
	{
		if (mailService != null && monsterRepository != null && client.getCervixDossier().getLaatsteScreeningRonde() != null)
		{
			CervixScreeningRonde laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			monsterRepository.findAllByOntvangstScreeningRondeAndBriefNotNull(laatsteScreeningRonde)
				.forEach(cervixMonster -> mailService.sendBMHKBezwaarControlleVerwijsAdviesMail(cervixMonster));
		}
	}

	private void verwerkBezwaarLichaamsmateriaal(Client client)
	{
		if (mailService != null && client.getCervixDossier().getLaatsteScreeningRonde() != null)
		{
			CervixScreeningRonde laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			monsterRepository.findAllByOntvangstScreeningRondeAndBriefNotNull(laatsteScreeningRonde)
				.forEach(cervixMonster -> mailService.sendBMHKBezwaarLichaamsmateriaalMailAsync(cervixMonster));
		}
	}
}
