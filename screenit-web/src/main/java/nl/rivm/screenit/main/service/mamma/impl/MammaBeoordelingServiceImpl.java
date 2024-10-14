package nl.rivm.screenit.main.service.mamma.impl;

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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.BeoordelingenReserveringResult;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseScreeningRondeService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DistributedLockService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingReserveringService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseVerslagService;
import nl.rivm.screenit.service.mamma.MammaHuisartsBerichtService;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
import nl.rivm.screenit.service.mamma.be.verslag.MammaVerslagDocumentCreator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.RevisionType;
import org.hibernate.envers.query.AuditEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBeoordelingServiceImpl implements MammaBeoordelingService
{

	@Autowired
	private MammaBeoordelingDao beoordelingDao;

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private MammaBaseBeoordelingReserveringService reserveringService;

	@Autowired
	private DistributedLockService distributedLockService;

	@Autowired
	private LogService logService;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private MammaHuisartsService huisartsService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private BerichtToBatchService hl7BerichtenToBatchService;

	@Autowired
	private MammaBaseVerslagService verslagService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private MammaBaseKansberekeningService kansberekeningService;

	@Autowired
	private BaseScreeningRondeService baseScreeningRondeService;

	@Override
	public List<MammaBeoordeling> getAlleBeoordelingenMetBeelden(MammaBeoordeling beoordeling)
	{
		return beoordelingDao.getAlleVorigeBeoordelingenMetBeelden(beoordeling);
	}

	@Override
	public List<MammaBeoordeling> getVorigeTweeTeTonenBeoordelingen(MammaBeoordeling beoordeling)
	{
		final var laatsteTweeBeoordelingen = beoordelingDao.getVorigeBeoordelingen(beoordeling, 2, true);
		if (laatsteTweeBeoordelingen.size() < 2 || isVerwijzing(laatsteTweeBeoordelingen.get(0)) || isVerwijzing(laatsteTweeBeoordelingen.get(1)))
		{
			return laatsteTweeBeoordelingen;
		}

		final var verwijzendeBeoordeling = beoordelingDao.getVorigeBeoordelingen(beoordeling, 1, false);
		if (verwijzendeBeoordeling.isEmpty())
		{
			return laatsteTweeBeoordelingen;
		}
		return Arrays.asList(laatsteTweeBeoordelingen.get(0), verwijzendeBeoordeling.get(0));
	}

	private boolean isVerwijzing(MammaBeoordeling mammaBeoordeling)
	{
		return mammaBeoordeling.getStatus() == MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
	}

	@Override
	public MammaLezing getOrCreate1eOf2eLezing(MammaBeoordeling beoordeling, InstellingGebruiker beoordelaar, boolean onervarenRadioloog)
	{
		MammaLezing lezing;

		if ((MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordeling.getStatus()) ||
			MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus())) &&
			beoordeling.getEersteLezing() != null)
		{
			lezing = beoordeling.getEersteLezing();
			lezing.setBeoordeling(beoordeling);
		}
		else if ((MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus())
			|| MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus())) &&
			beoordeling.getTweedeLezing() != null)
		{
			lezing = beoordeling.getTweedeLezing();
			lezing.setBeoordeling(beoordeling);
		}
		else
		{
			lezing = new MammaLezing();
			if (MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordeling.getStatus()))
			{
				lezing.setLezingType(MammaLezingType.EERSTE_LEZING);
			}
			else if (MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus()))
			{
				lezing.setLezingType(MammaLezingType.TWEEDE_LEZING);
			}
			lezing.setBiradsRechts(baseBeoordelingService.defaultBiradsWaarde(beoordeling, MammaZijde.RECHTER_BORST));
			lezing.setBiradsLinks(baseBeoordelingService.defaultBiradsWaarde(beoordeling, MammaZijde.LINKER_BORST));
			lezing.setBeoordelaar(beoordelaar);
			lezing.setOnervarenRadioloog(onervarenRadioloog);
			if (MammaOnderzoekType.TOMOSYNTHESE == beoordeling.getOnderzoek().getOnderzoekType() && lezing.getTomosyntheseRelevantVoorBeoordeling() == null)
			{
				lezing.setTomosyntheseRelevantVoorBeoordeling(false);
			}
		}
		return lezing;
	}

	@Override
	public MammaLezing getOrCreateDiscrepantieOfArbitrageLezing(MammaBeoordeling beoordeling, MammaLezingType huidigeLezingType, InstellingGebruiker gebruiker)
	{
		MammaLezing lezing;
		if (MammaLezingType.DISCREPANTIE_LEZING.equals(huidigeLezingType))
		{
			lezing = beoordeling.getDiscrepantieLezing();
		}
		else if (MammaLezingType.ARBITRAGE_LEZING.equals(huidigeLezingType))
		{
			lezing = beoordeling.getArbitrageLezing();
		}
		else
		{
			throw new IllegalArgumentException(huidigeLezingType + " is geen geldig lezing type.");
		}

		if (lezing == null)
		{
			lezing = new MammaLezing();
			lezing.setLezingType(huidigeLezingType);
			lezing.setBeoordelaar(gebruiker);
			lezing.setOnervarenRadioloog(!isBevoegdVoorArbitrage(gebruiker));
			lezing.setBiradsLinks(baseBeoordelingService.defaultBiradsWaarde(beoordeling, MammaZijde.LINKER_BORST));
			lezing.setBiradsRechts(baseBeoordelingService.defaultBiradsWaarde(beoordeling, MammaZijde.RECHTER_BORST));
			if (MammaOnderzoekType.TOMOSYNTHESE == beoordeling.getOnderzoek().getOnderzoekType() && lezing.getTomosyntheseRelevantVoorBeoordeling() == null)
			{
				lezing.setTomosyntheseRelevantVoorBeoordeling(false);
			}
		}
		return lezing;
	}

	@Override
	public Long getVolgendeBeoordelingId(Long huidigeBeoordelingId, List<Long> beoordelingenIds)
	{
		var huidigeBeoordelingIndex = beoordelingenIds.indexOf(huidigeBeoordelingId);
		return huidigeBeoordelingIndex + 1 < beoordelingenIds.size() ? beoordelingenIds.get(huidigeBeoordelingIndex + 1) : null;
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public BeoordelingenReserveringResult openBeschikbareBeoordeling(Long startBeoordelingId, List<Long> beoordelingenIds, InstellingGebruiker ingelogdeGebruiker,
		MammaBeLezerSoort lezerSoort)
	{
		if (startBeoordelingId == null)
		{
			return new BeoordelingenReserveringResult();
		}
		else if (MammaBeLezerSoort.CONCLUSIE_REVIEW.equals(lezerSoort))
		{
			return new BeoordelingenReserveringResult(List.of(startBeoordelingId));
		}
		else
		{
			var komendeGereserveerdeBeoordelingen = reserveerBeoordelingenBinnenLock(startBeoordelingId, beoordelingenIds, ingelogdeGebruiker, lezerSoort);

			var reserveringResult = new BeoordelingenReserveringResult(komendeGereserveerdeBeoordelingen);

			if (reserveringResult.getEersteGereserveerdeBeoordelingId() == null)
			{
				reserveringResult.setInfoMessage("Het beoordelen is afgerond, de laatste niet beoordeelde beoordeling is momenteel gereserveerd voor een collega.");
			}
			else if (!reserveringResult.getEersteGereserveerdeBeoordelingId().equals(startBeoordelingId))
			{
				reserveringResult.setInfoMessage("Er is een beoordeling overgeslagen die gereserveerd is voor een collega.");
			}
			return reserveringResult;
		}
	}

	private List<Long> reserveerBeoordelingenBinnenLock(Long startBeoordelingId, List<Long> beoordelingenIds, InstellingGebruiker ingelogdeGebruiker, MammaBeLezerSoort lezerSoort)
	{
		var locknaam = "BeoordelingenVoorBe/" + ingelogdeGebruiker.getOrganisatie().getId();
		try
		{

			distributedLockService.lockAndWait(locknaam, ingelogdeGebruiker);
			return reserveringService.reserveerBeoordelingen(startBeoordelingId, beoordelingenIds, ingelogdeGebruiker, lezerSoort);
		}
		finally
		{
			distributedLockService.unlock(locknaam, ingelogdeGebruiker);
		}
	}

	@Override
	public void radioloogHeeftGeenHandtekening(Gebruiker medewerker)
	{
		var email = preferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
		mailService.queueMailAanProfessional(email,
			"Handtekening van radioloog (BK) mist!!!",
			"De radioloog met gebruikersnaam '" + medewerker.getGebruikersnaam()
				+ "' wilde gaan screenen/beoordelen. Echter mist de handtekening en kan derhalve nu niet aan het werk. Dit moet zsm. opgelost worden iom. de regio.",
			MailPriority.HIGH);
	}

	@Override
	public MammaLezing[] getLezingenVoorVerslag(MammaBeoordeling beoordeling)
	{
		var lezingen = new MammaLezing[2];
		if (baseBeoordelingService.isLezingVerwijzen(beoordeling.getEersteLezing()) && baseBeoordelingService.isLezingVerwijzen(beoordeling.getTweedeLezing()))
		{
			lezingen[0] = beoordeling.getEersteLezing();
			lezingen[1] = beoordeling.getTweedeLezing();
		}
		else if (beoordeling.getDiscrepantieLezing() != null)
		{
			if (baseBeoordelingService.isLezingVerwijzen(beoordeling.getEersteLezing()))
			{
				lezingen[0] = beoordeling.getEersteLezing();
			}
			else if (baseBeoordelingService.isLezingVerwijzen(beoordeling.getTweedeLezing()))
			{
				lezingen[0] = beoordeling.getTweedeLezing();
			}
			if (beoordeling.getArbitrageLezing() != null)
			{
				lezingen[1] = beoordeling.getArbitrageLezing();
			}
			else
			{
				lezingen[1] = beoordeling.getDiscrepantieLezing();
			}
		}
		return lezingen;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void gunstigeUitslagMetNevenbevindingAfronden(MammaBeoordeling beoordeling, EnovationHuisarts alternativeHuisarts, InstellingGebruiker loggedInInstellingGebruiker)
	{
		baseBeoordelingService.verwerkBeoordelingStatusGunstigMetNevenbevindingen(beoordeling);
		verstuurHuisartsbericht(beoordeling, alternativeHuisarts, HuisartsBerichtType.MAMMA_UITSLAG_GUNSTIG_MET_NEVENBEVINDINGEN);
	}

	private void verstuurHuisartsbericht(MammaBeoordeling beoordeling, EnovationHuisarts alternativeHuisarts, HuisartsBerichtType berichtType)
	{
		var ronde = baseBeoordelingService.getScreeningRonde(beoordeling);
		var huisarts = ronde.getHuisarts();
		if (huisarts != null)
		{
			huisartsBerichtService.verstuurHuisartsBericht(beoordeling, huisarts, berichtType, false);
		}
		if (alternativeHuisarts != null)
		{
			huisartsBerichtService.verstuurHuisartsBericht(beoordeling, alternativeHuisarts, berichtType, false);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public File verslagGoedkeurenDoorCE(MammaBeoordeling beoordeling, boolean directPrinten, EnovationHuisarts alternatieveHuisarts, InstellingGebruiker ingelogdeGebruiker)
	{
		verslagVerwerkenDoorCE(beoordeling, ingelogdeGebruiker, MammaBeoordelingStatus.UITSLAG_ONGUNSTIG, LogGebeurtenis.MAMMA_CE_VERSLAG_GOEDKEUREN, "", "worden goedgekeurd");
		try
		{
			verslagService.verslagNaarFileStoreSchrijven(beoordeling);
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}

		hl7BerichtenToBatchService.queueMammaHL7v24BerichtUitgaand(baseBeoordelingService.getClientVanBeoordeling(beoordeling), MammaHL7v24ORMBerichtStatus.AUTHORISED);
		var screeningRonde = baseBeoordelingService.getScreeningRonde(beoordeling);
		var huisarts = screeningRonde.getHuisarts();
		var type = getOngunstigeUitslagBriefType(beoordeling, huisarts);
		var berichtType = HuisartsBerichtType.MAMMA_VERSLAG_UITSLAG_ONGUNSTIG;
		if (BriefType.getMammaFotobesprekingOngunstigeUitslagen().contains(type))
		{
			berichtType = HuisartsBerichtType.MAMMA_VERSLAG_UITSLAG_FOTOBESPREKING_ONGUNSTIG;
		}
		verstuurHuisartsbericht(beoordeling, alternatieveHuisarts, berichtType);
		var brief = briefService.maakBvoBrief(screeningRonde, type);
		if (directPrinten)
		{
			return maakBriefKlaarVoorAfdrukken(brief, getMammaVerslagDocumentCreator(beoordeling));
		}
		return null;
	}

	private MammaVerslagDocumentCreator getMammaVerslagDocumentCreator(MammaBeoordeling beoordeling)
	{
		beoordeling.getVerslagLezing().setBeoordeling(beoordeling);
		return new MammaVerslagDocumentCreator(beoordeling.getVerslagLezing());
	}

	@Override
	public void verslagAfkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker toegewezenRadioloog, InstellingGebruiker ingelogdeGebruiker)
	{
		if (toegewezenRadioloog != null)
		{
			baseBeoordelingService.wijsBeoordelingAanRadioloogToe(beoordeling, toegewezenRadioloog);
		}
		verslagVerwerkenDoorCE(beoordeling, ingelogdeGebruiker, MammaBeoordelingStatus.VERSLAG_AFGEKEURD, LogGebeurtenis.MAMMA_CE_VERSLAG_AFKEUREN,
			String.format("Verslag afgekeurd met reden: %s", beoordeling.getAfkeurreden()), "worden afgekeurd");
	}

	@Override
	public void verslagLaterGoedkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker)
	{
		verslagVerwerkenDoorCE(beoordeling, ingelogdeGebruiker, MammaBeoordelingStatus.VERSLAG_GOEDKEURING_OPGESCHORT, LogGebeurtenis.MAMMA_CE_VERSLAG_GOEDKEURING_OPGESCHORT, "",
			"later worden goedgekeurd");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void onbeoordeelbaarAfgehandeld(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker)
	{
		baseBeoordelingService.setStatus(beoordeling, MammaBeoordelingStatus.ONBEOORDEELBAAR);
		hibernateService.saveOrUpdate(beoordeling);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_CE_ONBEOORDEELBAAR_AFGEHANDELD, ingelogdeGebruiker, baseBeoordelingService.getClientVanBeoordeling(beoordeling), "",
			Bevolkingsonderzoek.MAMMA);
		kansberekeningService.dossierEventHerzien(baseBeoordelingService.getScreeningRonde(beoordeling).getDossier());
		var ronde = beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
		baseScreeningRondeService.screeningRondeAfronden(ronde);
	}

	private void verslagVerwerkenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker, MammaBeoordelingStatus nieuweStatus, LogGebeurtenis logGebeurtenis,
		String logGebeurtenisMelding, String exceptionExtension)
	{
		if (!MammaBeoordelingStatus.VERSLAG_GEREED.equals(beoordeling.getStatus()) && !MammaBeoordelingStatus.VERSLAG_GOEDKEURING_OPGESCHORT.equals(beoordeling.getStatus()))
		{
			throw new IllegalStateException("Beoordeling " + beoordeling.getId() + " heeft status " + beoordeling.getStatus() + ", dus het verslag kan niet "
				+ exceptionExtension + ".");
		}

		var screeningRonde = baseBeoordelingService.getScreeningRonde(beoordeling);
		huisartsService.koppelHuisarts(screeningRonde.getHuisarts(), screeningRonde, ingelogdeGebruiker);
		if (MammaBeoordelingStatus.UITSLAG_ONGUNSTIG == nieuweStatus)
		{
			baseBeoordelingService.verstuurXdsBericht(beoordeling);
			kansberekeningService.dossierEventHerzien(screeningRonde.getDossier());
		}
		baseBeoordelingService.setStatus(beoordeling, nieuweStatus);
		hibernateService.saveOrUpdate(beoordeling);
		logService.logGebeurtenis(logGebeurtenis, ingelogdeGebruiker, baseBeoordelingService.getClientVanBeoordeling(beoordeling), logGebeurtenisMelding,
			Bevolkingsonderzoek.MAMMA);
	}

	private BriefType getOngunstigeUitslagBriefType(MammaBeoordeling beoordeling, EnovationHuisarts huisarts)
	{
		var hoogsteBirads = baseBeoordelingService.getHoogsteBirads(beoordeling.getVerslagLezing());
		if (MammaBIRADSWaarde.EEN.equals(hoogsteBirads) || MammaBIRADSWaarde.TWEE.equals(hoogsteBirads))
		{
			var errorMelding = "Hoogste birads is gunstige uitslag, brieftype kan alleen bepaald worden voor verwijzende brieven.";
			LOG.error(errorMelding);
			throw new IllegalStateException(errorMelding);
		}
		var clientHeeftGunstigeUitslagbriefOntvangen = briefService.briefTypeAlVerstuurdInDezeRonde(baseBeoordelingService.getScreeningRonde(beoordeling),
			BriefType.getMammaGunstigeUitslagBriefTypen());
		var type = (clientHeeftGunstigeUitslagbriefOntvangen ? BriefType.MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5 : BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5);
		if (hoogsteBirads == MammaBIRADSWaarde.NUL)
		{
			type = (clientHeeftGunstigeUitslagbriefOntvangen ? BriefType.MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0 : BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0);
		}
		if (huisarts == null)
		{
			if (MammaBIRADSWaarde.VIER.equals(hoogsteBirads) || MammaBIRADSWaarde.VIJF.equals(hoogsteBirads))
			{
				type = (clientHeeftGunstigeUitslagbriefOntvangen ? BriefType.MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS
					: BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS);

			}
			else
			{
				type = (clientHeeftGunstigeUitslagbriefOntvangen ? BriefType.MAMMA_FOTOBESPREKING_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS
					: BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS);
			}
		}
		return type;
	}

	private File maakBriefKlaarVoorAfdrukken(MammaBrief brief, BaseDocumentCreator documentCreator)
	{
		File file = null;
		try
		{
			Consumer<MailMergeContext> context = (MailMergeContext mergeContext) -> mergeContext.putValue(MailMergeContext.CONTEXT_MAMMA_CE,
				clientService.bepaalCe(mergeContext.getClient()));
			if (BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS.equals(brief.getBriefType())
				|| BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS.equals(brief.getBriefType()))
			{
				file = briefService.maakPdfVanBrief(brief, documentCreator, context);
			}
			else
			{
				file = briefService.maakPdfVanBrief(brief, context);
			}
			briefService.setBriefGegenereerd(brief);
			hibernateService.saveOrUpdate(brief);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij mergen brief", e);
		}
		return file;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public File genereerPdfVoorOngunstigeUitslagBrief(MammaBeoordeling beoordeling)
	{
		var screeningRonde = baseBeoordelingService.getScreeningRonde(beoordeling);
		var brief = screeningRonde.getBrieven().stream()
			.filter(b -> BriefType.getMammaOngunstigeUitslagBriefTypen().contains(b.getBriefType())).findFirst()
			.orElse(null);
		if (brief != null)
		{
			return maakBriefKlaarVoorAfdrukken(brief, getMammaVerslagDocumentCreator(beoordeling));
		}
		return null;
	}

	@Override
	public List<Object[]> beoordelingGeschiedenis(MammaBeoordeling beoordeling)
	{
		var reader = AuditReaderFactory.get(hibernateService.getHibernateSession());
		var query = reader.createQuery().forRevisionsOfEntity(MammaBeoordeling.class, false, true);

		query.add(AuditEntity.id().eq(beoordeling.getId()));
		query.add(AuditEntity.revisionType().eq(RevisionType.MOD));
		query.add(AuditEntity.property("status")
			.in(Arrays.asList(MammaBeoordelingStatus.VERSLAG_GEREED, MammaBeoordelingStatus.VERSLAG_AFGEKEURD, MammaBeoordelingStatus.VERSLAG_MAKEN)));
		query.addOrder(AuditEntity.revisionNumber().desc());
		return query.getResultList();
	}

	@Override
	public boolean isLezingValide(MammaLezing lezing, List<LaesieDto> laesieDtos)
	{
		if ((heeftAnnotatieOpZijde(MammaZijde.RECHTER_BORST, laesieDtos) && !moetAnnotatieOpZijdeHebben(lezing.getBiradsRechts()))
			|| (!heeftAnnotatieOpZijde(MammaZijde.RECHTER_BORST, laesieDtos) && moetAnnotatieOpZijdeHebben(lezing.getBiradsRechts())))
		{
			return false;
		}
		return !((heeftAnnotatieOpZijde(MammaZijde.LINKER_BORST, laesieDtos) && !moetAnnotatieOpZijdeHebben(lezing.getBiradsLinks()))
			|| (!heeftAnnotatieOpZijde(MammaZijde.LINKER_BORST, laesieDtos) && moetAnnotatieOpZijdeHebben(lezing.getBiradsLinks())));
	}

	private boolean moetAnnotatieOpZijdeHebben(MammaBIRADSWaarde biradsWaarde)
	{
		return !MammaBIRADSWaarde.EEN.equals(biradsWaarde) && !MammaBIRADSWaarde.GEEN.equals(biradsWaarde);
	}

	private boolean heeftAnnotatieOpZijde(MammaZijde zijde, List<LaesieDto> laesieDtos)
	{
		return laesieDtos.stream().anyMatch(laesieDto -> laesieDto.getWelkeBorst().equals(zijde));
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean isBevoegdVoorArbitrage(InstellingGebruiker gebruiker)
	{
		return autorisatieService.getToegangLevel(gebruiker, Actie.TOEVOEGEN, true, Recht.GEBRUIKER_SCREENING_MAMMA_ARBITRAGE_WERKLIJST) != null;
	}

	@Override
	public List<MammaBeoordelingOpschortenReden> getMogelijkeOpschortRedenen(MammaBeoordeling beoordeling, MammaLezingType lezingType)
	{
		var onderzoek = beoordeling.getOnderzoek();
		var mogelijkeOpschortRedenen = new ArrayList<MammaBeoordelingOpschortenReden>();
		mogelijkeOpschortRedenen.add(MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN);

		if (lezingType == MammaLezingType.EERSTE_LEZING && onderzoek.getStatus() == MammaOnderzoekStatus.AFGEROND)
		{
			mogelijkeOpschortRedenen.add(MammaBeoordelingOpschortenReden.AANVULLENDE_BEELDEN_NODIG_SE);
		}
		if (onderzoek.getEerderMammogramZorginstelling() != null)
		{
			mogelijkeOpschortRedenen.add(MammaBeoordelingOpschortenReden.PRIORS_VAN_BUITEN_BVO);
		}
		return mogelijkeOpschortRedenen;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void logBeoordelingIngezien(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker, boolean isCoordinerendRadioloog)
	{
		if (isCoordinerendRadioloog)
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELING_INGEZIEN, ingelogdeGebruiker,
				baseBeoordelingService.getClientVanBeoordeling(beoordeling), "Co\u00F6rdinerend radioloog: inzage ronde review",
				Bevolkingsonderzoek.MAMMA);
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELING_INGEZIEN, ingelogdeGebruiker,
				baseBeoordelingService.getClientVanBeoordeling(beoordeling),
				Bevolkingsonderzoek.MAMMA);
		}
	}

}
