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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.document.sepa.CervixBetaalOpdrachtSpecificatieDocumentCreator;
import nl.rivm.screenit.dto.cervix.facturatie.CervixBetalingsZoekObject;
import nl.rivm.screenit.main.model.cervix.sepa.SEPACreditTransfer;
import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief_;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHerindexatieDto;
import nl.rivm.screenit.model.messagequeue.dto.VerwijderBetaalOpdrachtDto;
import nl.rivm.screenit.repository.cervix.CervixBetaalopdrachtRepository;
import nl.rivm.screenit.repository.cervix.CervixHuisartsTariefRepository;
import nl.rivm.screenit.repository.cervix.CervixLabTariefRepository;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.DistributedLockService;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseBetalingService;
import nl.rivm.screenit.service.cervix.CervixBaseVerrichtingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.validator.routines.IBANValidator;
import org.hibernate.HibernateException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.heeftNogGeenBetaalopdracht;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftHaTariefOverlap;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftLabTariefOverlap;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.heeftLabVoorTarief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isGroterOfGelijkAanGeldigTotEnMetHaTarief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isGroterOfGelijkAanGeldigTotEnMetLabTarief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isHuisartsTariefActief;
import static nl.rivm.screenit.specification.cervix.CervixTariefSpecification.isLabTariefActief;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.filterVerrichtingBinnenRegio;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.filterVerrichtingType;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.isKleinerDanVerrichtingsDatumVoorVerrichting;

@Slf4j
@Service
public class CervixBetalingServiceImpl implements CervixBetalingService
{
	private File template;

	private ExecutorService executorService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private CervixBaseVerrichtingService verrichtingService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private LogService logService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private MessageService messageService;

	@Autowired
	private DistributedLockService lockService;

	@Autowired
	private Cervix2023StartBepalingService cervix2023StartBepalingService;

	@Autowired
	private CervixBaseBetalingService baseBetalingService;

	@Autowired
	@Qualifier("cervixBetalingenDataProviderService")
	private CervixBetalingenDataProviderServiceImpl betalingenDataProviderService;

	@Autowired
	private CervixBetaalopdrachtRepository betaalopdrachtRepository;

	@Autowired
	private CervixHuisartsTariefRepository huisartsTariefRepository;

	@Autowired
	private CervixLabTariefRepository labTariefRepository;

	public CervixBetalingServiceImpl()
	{
		executorService = Executors.newSingleThreadExecutor();

		try (InputStream inputStream = getClass().getResourceAsStream("/aspose/BetalingSpecificatie.docx"))
		{
			template = File.createTempFile("BetalingSpecificatie", ".docx");
			FileUtils.copyInputStreamToFile(inputStream, template);
		}
		catch (IOException e)
		{
			LOG.error("Fout bij laden BetalingSpecificatie.docx", e);
		}
	}

	private String getBetalingsKenmerk(Date date)
	{
		var opdrachten = getVandaagGemaakteBetaalOpdrachten();
		var huidigeOpdrachtNummerVanVandaag = opdrachten.size() + 1;
		var kenmerknummer = StringUtils.leftPad(String.valueOf(huidigeOpdrachtNummerVanVandaag), 3, "0");
		return DateUtil.formatForPattern(Constants.DATE_FORMAT_YYYYMMDD, date) + kenmerknummer;
	}

	private List<CervixBetaalopdracht> getVandaagGemaakteBetaalOpdrachten()
	{
		var nu = currentDateSupplier.getLocalDate();
		return betaalopdrachtRepository.findByStatusDatumGreaterThanAndStatusDatumLessThan(DateUtil.toUtilDate(nu), DateUtil.toUtilDate(nu.plusDays(1)));
	}

	private String getSepaHash(File sepaBestand) throws IOException, NoSuchAlgorithmException
	{

		var md = MessageDigest.getInstance("SHA-256");
		var hexStringBuilder = new StringBuilder();
		try (FileInputStream fis = new FileInputStream(sepaBestand))
		{
			var dataBytes = new byte[1024];
			var nread = 0;
			while ((nread = fis.read(dataBytes)) != -1)
			{
				md.update(dataBytes, 0, nread);
			}
			var mdbytes = md.digest();
			for (byte mdbyte : mdbytes)
			{
				hexStringBuilder.append(Integer.toHexString(0xFF & mdbyte));
			}
		}
		return hexStringBuilder.toString();
	}

	private void syncVerrichtingen(CervixBetaalopdracht betaalopdracht)
	{
		betaalopdracht.getBetaalopdrachtRegels().stream()
			.filter(b -> b.getHuisartsLocatie() != null)
			.map(CervixBetaalopdrachtRegel::getSpecificaties).flatMap(Collection::stream)
			.map(CervixBetaalopdrachtRegelSpecificatie::getBoekRegels).flatMap(Collection::stream)
			.forEach(boekRegel ->
				huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(boekRegel.getVerrichting())));
	}

	private void berekenEinddatumCervixHuisartsTarief()
	{
		var tarieven = huisartsTariefRepository.findAll(isHuisartsTariefActief()
				.and(isGroterOfGelijkAanGeldigTotEnMetHaTarief(currentDateSupplier.getLocalDate())),
			Sort.by(Sort.Direction.ASC, CervixTarief_.GELDIG_VANAF_DATUM));
		berekenEinddatum(tarieven);
	}

	private void berekenEinddatumCervixLaboratoriumTarief(BMHKLaboratorium laboratorium)
	{
		var tarieven = labTariefRepository.findAll(isLabTariefActief()
				.and(heeftLabVoorTarief(laboratorium))
				.and(isGroterOfGelijkAanGeldigTotEnMetLabTarief(currentDateSupplier.getLocalDate())),
			Sort.by(Sort.Direction.ASC, CervixTarief_.GELDIG_VANAF_DATUM));
		berekenEinddatum(tarieven);
	}

	private void berekenEinddatum(List<? extends CervixTarief> tarieven)
	{
		for (int i = 0; i < tarieven.size(); i++)
		{
			var oldTarief = tarieven.get(i);
			if (i + 1 != tarieven.size())
			{
				var newTarief = tarieven.get(i + 1);
				oldTarief.setGeldigTotenmetDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(newTarief.getGeldigVanafDatum()).minusDays(1)));
			}
			else if (oldTarief.getGeldigTotenmetDatum() != null)
			{
				oldTarief.setGeldigTotenmetDatum(null);
			}
			hibernateService.saveOrUpdate(oldTarief);
		}
	}

	private String getLogMeldingHuisartsTariefVerwijderd(CervixHuisartsTarief huisartsTarief)
	{
		var logMelding = "Huisartstarief met bedrag: " + CervixTariefType.HUISARTS_UITSTRIJKJE.getBedragStringVanTarief(huisartsTarief) + " verwijderd. ";
		logMelding += "Tarief was" + CervixTariefUtil.getGeldigheidMelding(huisartsTarief);
		return logMelding;
	}

	private String getLogMeldingLabTariefVerwijderd(CervixLabTarief labTarief)
	{
		var logMelding = "Labtarief verwijderd met de bedragen: ";
		for (CervixTariefType labTariefType : getTariefTypenVoorLaboratorium(labTarief.getBmhkLaboratorium()))
		{
			logMelding += String.format("%s:  %s; ", labTariefType.getNaam(), labTariefType.getBedragStringVanTarief(labTarief));
		}
		logMelding += "Tarief was" + CervixTariefUtil.getGeldigheidMelding(labTarief);
		return logMelding;
	}

	private String queueHerindexeringVanVerrichtingen(CervixTarief nieuweTarief, List<CervixTarief> oudeTarieven)
	{
		var melding = "";
		for (CervixTarief oudeTarief : oudeTarieven)
		{
			if (!melding.isEmpty())
			{
				melding += "; ";
			}
			melding += baseBetalingService.getTariefString(oudeTarief);
			messageService.queueMessage(MessageType.HERINDEXATIE,
				new CervixHerindexatieDto(oudeTarief.getId(), nieuweTarief.getId(), CervixTariefType.isHuisartsTarief(nieuweTarief)));
		}
		return melding;
	}

	private String getLogMeldingHuisartsTarief(CervixHuisartsTarief nieuwTarief, String corrigeerOudeTarievenMelding)
	{
		var logMelding = "";
		if (corrigeerOudeTarievenMelding.isEmpty())
		{
			CervixTarief previousTarief = verrichtingService.getTariefVoorDatum(
				DateUtil.toUtilDate(DateUtil.toLocalDate(nieuwTarief.getGeldigVanafDatum()).minusDays(1)), null);
			logMelding = String.format("Van oud bedrag (%s) naar nieuw bedrag (%s); ", CervixTariefType.HUISARTS_UITSTRIJKJE.getBedragStringVanTarief(previousTarief),
				CervixTariefType.HUISARTS_UITSTRIJKJE.getBedragStringVanTarief(nieuwTarief));
			if (previousTarief != null)
			{
				logMelding += "Het oude tarief was" + CervixTariefUtil.getGeldigheidMelding(previousTarief) + ". ";
			}
			logMelding += "Het nieuwe tarief is" + CervixTariefUtil.getGeldigheidMelding(nieuwTarief);
		}
		else
		{
			logMelding = "Herindexering: " + corrigeerOudeTarievenMelding + ". Bedrag nieuw " + baseBetalingService.getTariefString(nieuwTarief);
		}
		return logMelding;
	}

	private void checkVoorGesplitsteOudeTarieven(List<CervixTarief> oudeTarieven, CervixTarief nieuweTarief)
	{
		for (CervixTarief tarief : oudeTarieven)
		{
			if (tarief.getGeldigVanafDatum().before(nieuweTarief.getGeldigVanafDatum()) && tarief.getGeldigTotenmetDatum() != null && nieuweTarief.getGeldigTotenmetDatum() != null
				&& tarief.getGeldigTotenmetDatum().after(nieuweTarief.getGeldigTotenmetDatum()))
			{
				throw new IllegalArgumentException("splitsen.niet.toegestaan");
			}
		}
	}

	private List<? extends CervixTarief> getOudeTarieven(CervixTarief nieuweTarief)
	{
		var isHuisartsTarief = CervixTariefType.isHuisartsTarief(nieuweTarief);
		var vanafDatum = DateUtil.toLocalDate(nieuweTarief.getGeldigVanafDatum()).plusDays(1);
		var totEnMetDatum = DateUtil.toLocalDate(nieuweTarief.getGeldigTotenmetDatum());
		if (totEnMetDatum != null)
		{
			totEnMetDatum = totEnMetDatum.minusDays(1);
		}
		if (isHuisartsTarief)
		{
			return huisartsTariefRepository.findAll(isHuisartsTariefActief()
					.and(heeftHaTariefOverlap(vanafDatum, totEnMetDatum)),
				Sort.by(Sort.Direction.ASC, CervixTarief_.GELDIG_VANAF_DATUM));
		}
		else
		{
			return labTariefRepository.findAll(isLabTariefActief()
					.and(heeftLabVoorTarief(((CervixLabTarief) nieuweTarief).getBmhkLaboratorium()))
					.and(heeftLabTariefOverlap(vanafDatum, totEnMetDatum)),
				Sort.by(Sort.Direction.ASC, CervixTarief_.GELDIG_VANAF_DATUM));
		}
	}

	private String corrigeerOudeTarieven(List<CervixTarief> tarieven, CervixTarief nieuweTarief)
	{
		var melding = "";
		for (CervixTarief oudeTarief : tarieven)
		{
			if (!melding.isEmpty())
			{
				melding += "; ";
			}
			if (oudeTarief.getGeldigVanafDatum().before(nieuweTarief.getGeldigVanafDatum()))
			{
				melding += "Oud " + baseBetalingService.getTariefString(oudeTarief);
				oudeTarief.setGeldigTotenmetDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(nieuweTarief.getGeldigVanafDatum()).minusDays(1)));
				melding += " is aangepast naar " + CervixTariefUtil.getGeldigheidMelding(oudeTarief);
			}
			else if (nieuweTarief.getGeldigTotenmetDatum() != null
				&& (oudeTarief.getGeldigTotenmetDatum() == null || (oudeTarief.getGeldigVanafDatum().before(nieuweTarief.getGeldigTotenmetDatum())
				&& oudeTarief.getGeldigTotenmetDatum().after(nieuweTarief.getGeldigTotenmetDatum()))))
			{
				melding += "Oud " + baseBetalingService.getTariefString(oudeTarief);
				oudeTarief.setGeldigVanafDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(nieuweTarief.getGeldigTotenmetDatum()).plusDays(1)));
				melding += " is aangepast naar " + CervixTariefUtil.getGeldigheidMelding(oudeTarief);
			}
			else
			{
				CervixTarief deproxiedTarief = (CervixTarief) HibernateHelper.deproxy(oudeTarief);
				if (CervixTariefType.isHuisartsTarief(oudeTarief))
				{
					melding += getLogMeldingHuisartsTariefVerwijderd((CervixHuisartsTarief) deproxiedTarief);
				}
				else
				{
					var labTarief = (CervixLabTarief) deproxiedTarief;
					melding += getLogMeldingLabTariefVerwijderd(labTarief);
				}
				oudeTarief.setActief(false);
			}

			hibernateService.saveOrUpdate(oudeTarief);
		}
		return melding;
	}

	private void toevoegenTariefAfronden(CervixTarief tarief, String corrigeerOudeTarievenMelding, Account account)
	{
		var melding = "";
		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			berekenEinddatumCervixHuisartsTarief();
			melding = getLogMeldingHuisartsTarief((CervixHuisartsTarief) tarief, corrigeerOudeTarievenMelding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTS_TARIEF_AANGEMAAKT, account, melding, Bevolkingsonderzoek.CERVIX);
		}
		else
		{
			var labTarief = (CervixLabTarief) tarief;
			var laboratorium = labTarief.getBmhkLaboratorium();
			berekenEinddatumCervixLaboratoriumTarief(laboratorium);
			melding = getLogMeldingLabTarief(labTarief, corrigeerOudeTarievenMelding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LAB_TARIEF_AANGEMAAKT, account, melding, Bevolkingsonderzoek.CERVIX);
		}
	}

	private String getLogMeldingLabTarief(CervixLabTarief nieuwTarief, String corrigeerOudeTarievenMelding)
	{
		var logMeldingBuilder = new StringBuilder();
		logMeldingBuilder.append("Laboratorium: ").append(nieuwTarief.getBmhkLaboratorium().getNaam());
		if (corrigeerOudeTarievenMelding.isEmpty())
		{
			var previousTarief = verrichtingService.getTariefVoorDatum(
				DateUtil.toUtilDate(DateUtil.toLocalDate(nieuwTarief.getGeldigVanafDatum()).minusDays(1)),
				nieuwTarief.getBmhkLaboratorium());
			for (CervixTariefType labTariefType : getTariefTypenVoorLaboratorium(nieuwTarief.getBmhkLaboratorium()))
			{
				var logMelding = String.format("; %s: Van oud bedrag (%s) naar nieuw bedrag %s", labTariefType.getNaam(),
					labTariefType.getBedragStringVanTarief(previousTarief),
					labTariefType.getBedragStringVanTarief(nieuwTarief));
				logMeldingBuilder.append(logMelding);
			}
			if (previousTarief != null)
			{
				logMeldingBuilder.append(". Het oude tarief was");
				logMeldingBuilder.append(CervixTariefUtil.getGeldigheidMelding(previousTarief));
			}
			logMeldingBuilder.append(". Het nieuwe tarief is");
			logMeldingBuilder.append(CervixTariefUtil.getGeldigheidMelding(nieuwTarief));
		}
		else
		{
			logMeldingBuilder.insert(0, "Indexering: ");
			logMeldingBuilder.append(corrigeerOudeTarievenMelding).append(" nieuwe bedragen ").append(baseBetalingService.getTariefString(nieuwTarief));
		}

		return logMeldingBuilder.toString();

	}

	@Override
	public List<CervixTariefType> getTariefTypenVoorLaboratorium(BMHKLaboratorium laboratorium)
	{
		var bmhk2023Lab = cervix2023StartBepalingService.isBmhk2023Laboratorium(laboratorium);
		return CervixTariefType.getAlleLabTariefTypes(bmhk2023Lab);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void genereerCervixBetalingsSpecificatieEnSepaBestand(Long betaalopdrachtId)
	{
		executorService.submit(new CervixBetalingsBestandenThread(betaalopdrachtId));
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Long opslaanBetaalopdracht(CervixBetaalopdracht opdracht, InstellingGebruiker ingelogedeGebruiker)
	{
		var nu = currentDateSupplier.getDate();
		opdracht.setStatusDatum(currentDateSupplier.getDate());
		if (StringUtils.isEmpty(opdracht.getBetalingskenmerk()))
		{
			opdracht.setBetalingskenmerk(getBetalingsKenmerk(nu));
		}
		opdracht.setStatus(BestandStatus.BEZIG_MET_VERWERKEN);
		opdracht.setSepaSpecificatiePdf(null);
		opdracht.setSepaDocument(null);
		opdracht.setVanIban(opdracht.getScreeningOrganisatie().getIban().toUpperCase());
		opdracht.setVanTenaamstelling(opdracht.getScreeningOrganisatie().getIbanTenaamstelling());

		hibernateService.saveOrUpdate(opdracht);

		var melding = "Screeningorganisatie: " + opdracht.getScreeningOrganisatie().getNaam() + "; Betalingskenmerk: " + opdracht.getBetalingskenmerk() + "; "
			+ opdracht.getOmschrijving();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_EXPORTEER_BETAALOPDRACHT, ingelogedeGebruiker, melding, Bevolkingsonderzoek.CERVIX);

		for (CervixBetaalopdrachtRegel regel : opdracht.getBetaalopdrachtRegels())
		{

			if (IBANValidator.getInstance().isValid(regel.getNaarIban()))
			{
				hibernateService.saveOrUpdate(regel);
				for (CervixBetaalopdrachtRegelSpecificatie spec : regel.getSpecificaties())
				{
					hibernateService.saveOrUpdate(spec);
					for (CervixBoekRegel boekRegel : spec.getBoekRegels())
					{
						boekRegel.setSpecificatie(spec);
						hibernateService.saveOrUpdate(boekRegel);
					}
				}
			}
			else
			{
				var ibanMelding = "";
				List<Instelling> instellingen = new ArrayList<>();
				if (regel.getHuisartsLocatie() != null)
				{
					ibanMelding = String.format("Foutief IBAN voor AGB: %s, locatie: %s", regel.getHuisartsLocatie().getHuisarts().getAgbcode(),
						regel.getHuisartsLocatie().getNaam());
					instellingen.add(opdracht.getScreeningOrganisatie());
				}
				else if (regel.getLaboratorium() != null)
				{
					ibanMelding = String.format("Foutief IBAN voor BMHK Laboratorium: %s", regel.getLaboratorium().getNaam());
				}

				logService.logGebeurtenis(LogGebeurtenis.CERVIX_BETALING_GENEREREN_IBAN_FOUT, instellingen, ingelogedeGebruiker, ibanMelding, Bevolkingsonderzoek.CERVIX);
			}
		}
		return opdracht.getId();
	}

	@Override
	public void maakSepaBestand(File sepaBestand, CervixBetaalopdracht betaalOpdracht)
	{
		try (var fileOutputStream = new FileOutputStream(sepaBestand))
		{
			Date nu = currentDateSupplier.getDate();

			var transfer = new SEPACreditTransfer();
			transfer.buildGroupHeader(String.valueOf(betaalOpdracht.getId()), betaalOpdracht.getVanTenaamstelling(), nu);

			var betaalOpdrachtRegels = betaalOpdracht.getBetaalopdrachtRegels();

			SEPACreditTransfer.Betaalgroep betaalgroep = null;

			if (!betaalOpdrachtRegels.isEmpty())
			{

				betaalgroep = transfer.betaalgroep(String.valueOf(betaalOpdrachtRegels.get(0).getId()),
					betaalOpdracht.getStatusDatum() == null ? currentDateSupplier.getLocalDate() : DateUtil.toLocalDate(betaalOpdracht.getStatusDatum()),
					betaalOpdracht.getVanTenaamstelling(), betaalOpdracht.getVanIban(), null);

				for (var regel : betaalOpdracht.getBetaalopdrachtRegels())
				{
					betaalgroep.creditTransfer(betaalOpdracht.getBetalingskenmerk(), regel.getBedrag(), null, regel.getNaarTenaamstelling(), regel.getNaarIban(),
						betaalOpdracht.getOmschrijving());
				}
			}
			transfer.write(fileOutputStream);
		}
		catch (HibernateException e)
		{
			LOG.error("Er is een Hibernate Exception opgetreden met het genereren van het sepa bestand.", e);
		}
		catch (Exception e)
		{
			LOG.error("Er is een onverwachte fout opgetreden met het genereren van het sepa bestand.", e);
			betaalOpdracht.setStatus(BestandStatus.CRASH);
			hibernateService.saveOrUpdate(betaalOpdracht);
		}
	}

	@Override
	public void maakSpecificatieBestand(File specificatieBestand, CervixBetaalopdracht opdracht) throws Exception
	{
		try (var stream = new FileOutputStream(specificatieBestand))
		{
			var context = new MailMergeContext();
			context.putValue(MailMergeContext.CONTEXT_SCREENING_ORGANISATIE, opdracht.getScreeningOrganisatie());
			context.putValue(MailMergeContext.CONTEXT_BMHK_BETAALOPDRACHT, opdracht);
			var document = asposeService.processDocumentWithCreator(context, template, new CervixBetaalOpdrachtSpecificatieDocumentCreator(opdracht), true);
			document.save(stream, asposeService.getPdfSaveOptions());
		}
		catch (HibernateException e)
		{
			LOG.error("Er is een HibernateException opgetreden met het genereren van het sepa specificatie.", e);
		}
		catch (Exception e)
		{
			LOG.error("Er is een onverwachte fout opgetreden met het genereren van het sepa specificatie.", e);
			opdracht.setStatus(BestandStatus.CRASH);
			hibernateService.saveOrUpdate(opdracht);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderSepaBestanden(CervixBetaalopdracht betaalopdracht)
	{
		betaalopdracht.setStatus(BestandStatus.BEZIG_MET_VERWIJDEREN);
		hibernateService.saveOrUpdate(betaalopdracht);

		messageService.queueMessage(MessageType.VERWIJDER_BETAAL_OPDRACHT,
			new VerwijderBetaalOpdrachtDto(betaalopdracht.getId()));
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void archiveerBestaandeOpdrachten(ScreeningOrganisatie screeningOrganisatie)
	{
		Map<String, Object> params = new HashMap<>();
		params.put("status", BestandStatus.VERWERKT);
		params.put("screeningOrganisatie", screeningOrganisatie);
		var betalingsOpdrachten = hibernateService.getByParameters(CervixBetaalopdracht.class, params);
		for (var betaalopdracht : betalingsOpdrachten)
		{
			betaalopdracht.setStatus(BestandStatus.GEARCHIVEERD);
			hibernateService.saveOrUpdate(betaalopdracht);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void toevoegenTarief(CervixTarief tarief, Account account)
	{
		hibernateService.saveOrUpdate(tarief);
		toevoegenTariefAfronden(tarief, "", account);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED, rollbackFor = IllegalArgumentException.class)
	public String toevoegenIndexatieTarief(CervixTarief nieuweTarief, Account account)
	{
		List<CervixTarief> oudeTarieven = new ArrayList<>();
		oudeTarieven.addAll(getOudeTarieven(nieuweTarief));
		checkVoorGesplitsteOudeTarieven(oudeTarieven, nieuweTarief);
		hibernateService.saveOrUpdate(nieuweTarief);

		var corrigeerOudeTarievenMelding = corrigeerOudeTarieven(oudeTarieven, nieuweTarief);

		toevoegenTariefAfronden(nieuweTarief, corrigeerOudeTarievenMelding, account);
		return queueHerindexeringVanVerrichtingen(nieuweTarief, oudeTarieven);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderCervixTarief(CervixTarief tarief, Account account)
	{
		tarief.setActief(Boolean.FALSE);
		hibernateService.saveOrUpdate(tarief);
		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			var melding = getLogMeldingHuisartsTariefVerwijderd((CervixHuisartsTarief) HibernateHelper.deproxy(tarief));
			berekenEinddatumCervixHuisartsTarief();
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTS_TARIEF_VERWIJDERD, account, melding, Bevolkingsonderzoek.CERVIX);
		}
		else
		{
			var labTarief = (CervixLabTarief) HibernateHelper.deproxy(tarief);
			var verwijderdMelding = getLogMeldingLabTariefVerwijderd(labTarief);
			var melding = "Laboratorium: " + labTarief.getBmhkLaboratorium().getNaam() + verwijderdMelding;
			berekenEinddatumCervixLaboratoriumTarief(CervixTariefType.getLabTarief(tarief).getBmhkLaboratorium());
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LAB_TARIEF_VERWIJDERD, account, melding, Bevolkingsonderzoek.CERVIX);
		}
	}

	@Override
	public List<CervixBoekRegel> getVerrichtingenVoorBetaling(CervixBetalingsZoekObject zoekObject)
	{
		var session = hibernateService.getHibernateSession();

		var cb = session.getCriteriaBuilder();
		var q = cb.createQuery(CervixBoekRegel.class);
		var r = q.from(CervixVerrichting.class);

		q.select(r.get(CervixVerrichting_.laatsteBoekRegel))
			.where(filterVerrichtingType(zoekObject.isVerrichtingenHuisarts(), zoekObject.isVerrichtingenLaboratorium())
				.and(isKleinerDanVerrichtingsDatumVoorVerrichting(DateUtil.toLocalDate(zoekObject.getVerrichtingsdatumTotEnMet())))
				.and(filterVerrichtingBinnenRegio(zoekObject.getScreeningOrganisatieId()))
				.and(heeftNogGeenBetaalopdracht().toSpecification(CervixVerrichting_.laatsteBoekRegel))
				.toPredicate(r, q, cb));

		return session.createQuery(q).getResultList();
	}

	@Override
	public List<CervixBetaalopdracht> getBetaalOpdrachten(Sort sort, long first, long count)
	{
		return betalingenDataProviderService.findPage(first, count, null, sort);
	}

	@Override
	public Long countBetaalOpdrachten()
	{
		return betalingenDataProviderService.size(null);
	}

	private class CervixBetalingsBestandenThread extends OpenHibernate5SessionInThread
	{
		@Autowired
		private CervixBetalingService cervixBetalingService;

		private CervixBetaalopdracht betaalopdracht;

		private final Long betaalopdrachtId;

		CervixBetalingsBestandenThread(Long betaalopdrachtId)
		{
			this.betaalopdrachtId = betaalopdrachtId;
		}

		@Override
		protected void runInternal()
		{
			try
			{
				betaalopdracht = hibernateService.load(CervixBetaalopdracht.class, betaalopdrachtId);

				var specificatieNaam = betaalopdracht.getBetalingskenmerk() + "-specificatie";
				var specificatieSuffix = "pdf";
				File specificatie = File.createTempFile(specificatieNaam, specificatieSuffix);
				cervixBetalingService.maakSpecificatieBestand(specificatie, betaalopdracht);

				var document = new UploadDocument();
				document.setActief(true);
				document.setNaam(specificatieNaam + "." + specificatieSuffix);
				document.setContentType("application/pdf");
				document.setFile(specificatie);
				uploadDocumentService.saveOrUpdate(document, FileStoreLocation.CERVIX_BETALING_PDF, betaalopdracht.getId());
				betaalopdracht.setSepaSpecificatiePdf(document);

				var sepaNaam = betaalopdracht.getBetalingskenmerk() + "-sepa";
				var sepaSuffix = "xml";
				File sepaBestand = File.createTempFile(sepaNaam, sepaSuffix);
				cervixBetalingService.maakSepaBestand(sepaBestand, betaalopdracht);
				var hashtotaal = getSepaHash(sepaBestand);

				document = new UploadDocument();
				document.setActief(true);
				document.setNaam(sepaNaam + "." + sepaSuffix);
				document.setContentType("application/pdf");
				document.setFile(sepaBestand);
				uploadDocumentService.saveOrUpdate(document, FileStoreLocation.CERVIX_BETALING_SEPA, betaalopdracht.getId());
				betaalopdracht.setSepaDocument(document);
				betaalopdracht.setHashtotaal(hashtotaal);
				betaalopdracht.setStatus(BestandStatus.VERWERKT);
				hibernateService.saveOrUpdate(betaalopdracht);
				syncVerrichtingen(betaalopdracht);
			}
			catch (HibernateException e)
			{
				LOG.error("Er is iets misgegaan met het opslaan van betaalopdracht in de database", e);
			}
			catch (IOException e)
			{
				LOG.error("Er is iets misgegaan met de filestore", e);
				betaalopdracht.setStatus(BestandStatus.CRASH);
				hibernateService.saveOrUpdate(betaalopdracht);
			}
			catch (Exception e)
			{
				LOG.error("Er is iets misgegaan met het aanmaken van de bestanden", e);
				betaalopdracht.setStatus(BestandStatus.CRASH);
				hibernateService.saveOrUpdate(betaalopdracht);
			}
			finally
			{
				lockService.unlock(Constants.BMHK_BETALING_GENEREREN_LOCKNAAM);
			}
		}
	}
}
