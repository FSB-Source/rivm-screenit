package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
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

import javax.xml.bind.JAXBException;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.document.sepa.CervixBetaalOpdrachtSpecificatieDocumentCreator;
import nl.rivm.screenit.main.model.cervix.sepa.SEPACreditTransfer;
import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.web.ScreenitSession;
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
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHerindexatieDto;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.validator.routines.IBANValidator;
import org.hibernate.HibernateException;
import org.joda.time.LocalDate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;
import com.fasterxml.jackson.core.JsonProcessingException;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixBetalingServiceImpl implements CervixBetalingService
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixBetalingServiceImpl.class);

	private File template = null;

	private ExecutorService executorService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private CervixVerrichtingDao verrichtingDao;

	@Autowired
	private CervixVerrichtingService verrichtingService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private MessageService messageService;

	public CervixBetalingServiceImpl()
	{
		executorService = Executors.newSingleThreadExecutor();
		template = new File(getClass().getClassLoader().getResource("aspose/BetalingSpecificatie.docx").getFile());
	}

	private String getBetalingsKenmerk(Date date)
	{
		List<CervixBetaalopdracht> opdrachten = verrichtingDao.getVandaagGemaakteBetaalOpdrachten();
		int huidigeOpdrachtNummerVanVandaag = opdrachten.size() + 1;
		String kenmerknummer = StringUtils.leftPad(String.valueOf(huidigeOpdrachtNummerVanVandaag), 3, "0");
		return DateUtil.formatForPattern(Constants.DATE_FORMAT_YYYYMMDD, date) + kenmerknummer;
	}

	@Override
	public void maakSepaBestand(File sepaBestand, CervixBetaalopdracht betaalOpdracht) throws JAXBException, FileNotFoundException
	{
		try (FileOutputStream fileOutputStream = new FileOutputStream(sepaBestand))
		{
			Date nu = currentDateSupplier.getDate();

			SEPACreditTransfer transfer = new SEPACreditTransfer();
			transfer.buildGroupHeader(String.valueOf(betaalOpdracht.getId()), betaalOpdracht.getVanTenaamstelling(), nu);

			List<CervixBetaalopdrachtRegel> betaalOpdrachtRegels = betaalOpdracht.getBetaalopdrachtRegels();

			SEPACreditTransfer.Betaalgroep betaalgroep = null;

			if (betaalOpdrachtRegels.size() > 0)
			{

				betaalgroep = transfer.betaalgroep(String.valueOf(betaalOpdrachtRegels.get(0).getId()), new LocalDate(betaalOpdracht.getStatusDatum()),
					betaalOpdracht.getVanTenaamstelling(), betaalOpdracht.getVanIban(),
					null);

				for (CervixBetaalopdrachtRegel regel : betaalOpdracht.getBetaalopdrachtRegels())
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
		try (FileOutputStream stream = new FileOutputStream(specificatieBestand))
		{
			MailMergeContext context = new MailMergeContext();
			context.putValue(MailMergeContext.CONTEXT_SCREENING_ORGANISATIE, opdracht.getScreeningOrganisatie());
			context.putValue(MailMergeContext.CONTEXT_BMHK_BETAALOPDRACHT, opdracht);
			Document document = asposeService.processDocumentWithCreator(context, template, new CervixBetaalOpdrachtSpecificatieDocumentCreator(opdracht), true);
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
	public Long opslaanBetaalopdracht(CervixBetaalopdracht opdracht)
	{
		Date nu = currentDateSupplier.getDate();
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

		String melding = "Screeningorganisatie: " + opdracht.getScreeningOrganisatie().getNaam() + "; Betalingskenmerk: " + opdracht.getBetalingskenmerk() + "; "
			+ opdracht.getOmschrijving();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_EXPORTEER_BETAALOPDRACHT, ScreenitSession.get().getLoggedInAccount(), melding, Bevolkingsonderzoek.CERVIX);

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
				String ibanMelding = "";
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

				logService.logGebeurtenis(LogGebeurtenis.CERVIX_BETALING_GENEREREN_IBAN_FOUT, instellingen, null, ibanMelding, Bevolkingsonderzoek.CERVIX);
			}
		}
		return opdracht.getId();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void genereerCervixBetalingsSpecificatieEnSepaBestand(Long betaalopdrachtId)
	{
		executorService.submit(new CervixBetalingsBestandenThread(betaalopdrachtId));
	}

	private class CervixBetalingsBestandenThread extends OpenHibernate5SessionInThread
	{
		@Autowired
		private CervixBetalingService cervixBetalingService;

		private CervixBetaalopdracht betaalopdracht;

		private Long betaalopdrachtId;

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

				String specificatieNaam = betaalopdracht.getBetalingskenmerk() + "-specificatie";
				String specificatieSuffix = "pdf";
				File specificatie = File.createTempFile(specificatieNaam, specificatieSuffix);
				cervixBetalingService.maakSpecificatieBestand(specificatie, betaalopdracht);

				UploadDocument document = new UploadDocument();
				document.setActief(true);
				document.setNaam(specificatieNaam + "." + specificatieSuffix);
				document.setContentType("application/pdf");
				document.setFile(specificatie);
				fileService.saveOrUpdateUploadDocument(document, FileStoreLocation.CERVIX_BETALING_PDF, betaalopdracht.getId());
				betaalopdracht.setSepaSpecificatiePdf(document);

				String sepaNaam = betaalopdracht.getBetalingskenmerk() + "-sepa";
				String sepaSuffix = "xml";
				File sepaBestand = File.createTempFile(sepaNaam, sepaSuffix);
				cervixBetalingService.maakSepaBestand(sepaBestand, betaalopdracht);
				String hashtotaal = getSepaHash(sepaBestand);

				document = new UploadDocument();
				document.setActief(true);
				document.setNaam(sepaNaam + "." + sepaSuffix);
				document.setContentType("application/pdf");
				document.setFile(sepaBestand);
				fileService.saveOrUpdateUploadDocument(document, FileStoreLocation.CERVIX_BETALING_SEPA, betaalopdracht.getId());
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
		}
	}

	private String getSepaHash(File sepaBestand) throws IOException, NoSuchAlgorithmException
	{

		MessageDigest md = MessageDigest.getInstance("SHA-256");
		StringBuilder hexStringBuilder = new StringBuilder();
		try (FileInputStream fis = new FileInputStream(sepaBestand))
		{
			byte[] dataBytes = new byte[1024];
			int nread = 0;
			while ((nread = fis.read(dataBytes)) != -1)
			{
				md.update(dataBytes, 0, nread);
			}
			byte[] mdbytes = md.digest();
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
			.forEach(boekRegel -> {
				huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(boekRegel.getVerrichting()));
			});
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderSepaBestanden(CervixBetaalopdracht betaalopdracht, InstellingGebruiker loggedInInstellingGebruiker)
	{
		for (CervixBetaalopdrachtRegel opdrachtRegel : betaalopdracht.getBetaalopdrachtRegels())
		{
			for (CervixBetaalopdrachtRegelSpecificatie specificatie : opdrachtRegel.getSpecificaties())
			{
				for (CervixBoekRegel boekRegel : specificatie.getBoekRegels())
				{
					boekRegel.setSpecificatie(null);
					hibernateService.saveOrUpdate(boekRegel);
					if (boekRegel.getVerrichting().getType() == CervixTariefType.HUISARTS_UITSTRIJKJE)
					{
						huisartsenportaalSyncService.sendJmsBericht(CervixHuisartsToDtoUtil.getVerrichtingDto(boekRegel.getVerrichting()));
					}
				}
				specificatie.getBoekRegels().clear();
			}
		}

		UploadDocument sepaDocument = betaalopdracht.getSepaDocument();

		UploadDocument sepaSpecificatiePdf = betaalopdracht.getSepaSpecificatiePdf();
		betaalopdracht.setSepaDocument(null);
		betaalopdracht.setSepaSpecificatiePdf(null);
		if (sepaDocument != null)
		{
			fileService.delete(sepaDocument, true);
		}
		if (sepaSpecificatiePdf != null)
		{
			fileService.delete(sepaSpecificatiePdf, true);
		}
		hibernateService.delete(betaalopdracht);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_BETAALOPDRACHT_VERWIJDERD, loggedInInstellingGebruiker, "Betalingskenmerk: " + betaalopdracht.getBetalingskenmerk(),
			Bevolkingsonderzoek.CERVIX);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void archiveerBestaandeOpdrachten(ScreeningOrganisatie screeningOrganisatie)
	{
		Map<String, Object> params = new HashMap<>();
		params.put("status", BestandStatus.VERWERKT);
		params.put("screeningOrganisatie", screeningOrganisatie);
		List<CervixBetaalopdracht> betalingsOpdrachten = hibernateService.getByParameters(CervixBetaalopdracht.class, params);
		for (CervixBetaalopdracht betaalopdracht : betalingsOpdrachten)
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

	private void berekenEinddatumCervixHuisartsTarief()
	{
		List<CervixHuisartsTarief> tarieven = verrichtingDao.getCervixHuisartsTarievenZonderEinddatum();
		berekenEinddatum(tarieven);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void berekenEinddatumCervixLaboratoriumTarief(BMHKLaboratorium laboratorium)
	{
		List<CervixLabTarief> tarieven = verrichtingDao.getCervixLabTarievenZonderEinddatum(laboratorium);
		berekenEinddatum(tarieven);
	}

	private void berekenEinddatum(List<? extends CervixTarief> tarieven)
	{
		for (int i = 0; i < tarieven.size(); i++)
		{
			CervixTarief oldTarief = tarieven.get(i);
			if (i + 1 != tarieven.size())
			{
				CervixTarief newTarief = tarieven.get(i + 1);
				oldTarief.setGeldigTotenmetDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(newTarief.getGeldigVanafDatum()).minusDays(1)));
			}
			else if (oldTarief.getGeldigTotenmetDatum() != null)
			{
				oldTarief.setGeldigTotenmetDatum(null);
			}
			hibernateService.saveOrUpdate(oldTarief);
		}
	}

	@Override
	public void verwijderCervixTarief(CervixTarief tarief, Account account)
	{
		tarief.setActief(Boolean.FALSE);
		hibernateService.saveOrUpdate(tarief);
		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			String melding = getLogMeldingHuisartsTariefVerwijderd((CervixHuisartsTarief) HibernateHelper.deproxy(tarief));
			berekenEinddatumCervixHuisartsTarief();
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTS_TARIEF_VERWIJDERD, account, melding, Bevolkingsonderzoek.CERVIX);
		}
		else
		{
			CervixLabTarief labTarief = (CervixLabTarief) HibernateHelper.deproxy(tarief);
			String verwijderdMelding = getLogMeldingLabTariefVerwijderd(labTarief);
			String melding = "Laboratorium: " + labTarief.getBmhkLaboratorium().getNaam() + verwijderdMelding;
			berekenEinddatumCervixLaboratoriumTarief(CervixTariefType.getLabTarief(tarief).getBmhkLaboratorium());
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LAB_TARIEF_VERWIJDERD, account, melding, Bevolkingsonderzoek.CERVIX);
		}
	}

	private String getLogMeldingHuisartsTariefVerwijderd(CervixHuisartsTarief huisartsTarief)
	{
		String logMelding = "Huisartstarief met bedrag: " + CervixTariefType.HUISARTS_UITSTRIJKJE.getBedragStringVanTarief(huisartsTarief) + " verwijderd. ";
		logMelding += "Tarief was" + CervixTariefUtil.getGeldigheidMelding(huisartsTarief);
		return logMelding;
	}

	private String getLogMeldingLabTariefVerwijderd(CervixLabTarief labTarief)
	{
		String logMelding = "Labtarief verwijderd met de bedragen: ";
		for (CervixTariefType labTariefType : CervixTariefType.getAlleLabTariefTypes())
		{
			logMelding += String.format("%s:  %s; ", labTariefType.getNaam(), labTariefType.getBedragStringVanTarief(labTarief));
		}
		logMelding += "Tarief was" + CervixTariefUtil.getGeldigheidMelding(labTarief);
		return logMelding;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED, rollbackFor = IllegalArgumentException.class)
	public String toevoegenIndexatieTarief(CervixTarief nieuweTarief, Account account) throws JsonProcessingException
	{
		List<CervixTarief> oudeTarieven = new ArrayList<>();
		oudeTarieven.addAll(getOudeTarieven(nieuweTarief));
		checkVoorGesplitsteOudeTarieven(oudeTarieven, nieuweTarief);
		hibernateService.saveOrUpdate(nieuweTarief);
		String corrigeerOudeTarievenMelding = corrigeerOudeTarieven(oudeTarieven, nieuweTarief);

		toevoegenTariefAfronden(nieuweTarief, corrigeerOudeTarievenMelding, account);
		return queueHerindexeringVanVerrichtingen(nieuweTarief, oudeTarieven);
	}

	private String queueHerindexeringVanVerrichtingen(CervixTarief nieuweTarief, List<CervixTarief> oudeTarieven) throws JsonProcessingException
	{
		String melding = "";
		for (CervixTarief oudeTarief : oudeTarieven)
		{
			if (!melding.isEmpty())
			{
				melding += "; ";
			}
			melding += CervixTariefUtil.getTariefString(oudeTarief);
			messageService.queueMessage(MessageType.HERINDEXATIE,
				new CervixHerindexatieDto(oudeTarief.getId(), nieuweTarief.getId(), CervixTariefType.isHuisartsTarief(nieuweTarief)));
		}
		return melding;
	}

	private String getLogMeldingHuisartsTarief(CervixHuisartsTarief nieuwTarief, String corrigeerOudeTarievenMelding)
	{
		String logMelding = "";
		if (corrigeerOudeTarievenMelding.isEmpty())
		{
			CervixTarief previousTarief = verrichtingService.getTariefVoorDatum(CervixTariefType.HUISARTS_UITSTRIJKJE,
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
			logMelding = "Herindexering: " + corrigeerOudeTarievenMelding + ". Bedrag nieuw " + CervixTariefUtil.getTariefString(nieuwTarief);
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

	private List<CervixTarief> getOudeTarieven(CervixTarief nieuweTarief)
	{
		boolean isHuisartsTarief = CervixTariefType.isHuisartsTarief(nieuweTarief);
		if (isHuisartsTarief)
		{
			return verrichtingDao.getHuisartsTarievenTussen(nieuweTarief.getGeldigVanafDatum(), nieuweTarief.getGeldigTotenmetDatum());
		}
		else
		{
			return verrichtingDao.getLabTarievenTussen(((CervixLabTarief) nieuweTarief).getBmhkLaboratorium(), nieuweTarief.getGeldigVanafDatum(),
				nieuweTarief.getGeldigTotenmetDatum());
		}
	}

	private String corrigeerOudeTarieven(List<CervixTarief> tarieven, CervixTarief nieuweTarief)
	{
		String melding = "";
		for (CervixTarief oudeTarief : tarieven)
		{
			if (!melding.isEmpty())
			{
				melding += "; ";
			}
			if (oudeTarief.getGeldigVanafDatum().before(nieuweTarief.getGeldigVanafDatum()))
			{
				melding += "Oud " + CervixTariefUtil.getTariefString(oudeTarief);
				oudeTarief.setGeldigTotenmetDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(nieuweTarief.getGeldigVanafDatum()).minusDays(1)));
				melding += " is aangepast naar " + CervixTariefUtil.getGeldigheidMelding(oudeTarief);
			}
			else if (nieuweTarief.getGeldigTotenmetDatum() != null
				&& (oudeTarief.getGeldigTotenmetDatum() == null || (oudeTarief.getGeldigVanafDatum().before(nieuweTarief.getGeldigTotenmetDatum())
					&& oudeTarief.getGeldigTotenmetDatum().after(nieuweTarief.getGeldigTotenmetDatum()))))
			{
				melding += "Oud " + CervixTariefUtil.getTariefString(oudeTarief);
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
					melding += getLogMeldingLabTariefVerwijderd((CervixLabTarief) deproxiedTarief);
				}
				oudeTarief.setActief(false);
			}

			hibernateService.saveOrUpdate(oudeTarief);
		}
		return melding;
	}

	private void toevoegenTariefAfronden(CervixTarief tarief, String corrigeerOudeTarievenMelding, Account account)
	{
		String melding = "";
		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			berekenEinddatumCervixHuisartsTarief();
			melding = getLogMeldingHuisartsTarief((CervixHuisartsTarief) tarief, corrigeerOudeTarievenMelding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTS_TARIEF_AANGEMAAKT, account, melding, Bevolkingsonderzoek.CERVIX);
		}
		else
		{
			CervixLabTarief labTarief = (CervixLabTarief) tarief;
			BMHKLaboratorium laboratorium = labTarief.getBmhkLaboratorium();
			berekenEinddatumCervixLaboratoriumTarief(laboratorium);
			melding = getLogMeldingLabTarief(labTarief, corrigeerOudeTarievenMelding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LAB_TARIEF_AANGEMAAKT, account, melding, Bevolkingsonderzoek.CERVIX);
		}
	}

	private String getLogMeldingLabTarief(CervixLabTarief nieuwTarief, String corrigeerOudeTarievenMelding)
	{
		StringBuilder logMeldingBuilder = new StringBuilder();
		logMeldingBuilder.append("Laboratorium: ").append(nieuwTarief.getBmhkLaboratorium().getNaam());
		if (corrigeerOudeTarievenMelding.isEmpty())
		{
			CervixTarief previousTarief = verrichtingService.getTariefVoorDatum(CervixTariefType.LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE,
				DateUtil.toUtilDate(DateUtil.toLocalDate(nieuwTarief.getGeldigVanafDatum()).minusDays(1)),
				nieuwTarief.getBmhkLaboratorium());
			for (CervixTariefType labTariefType : CervixTariefType.getAlleLabTariefTypes())
			{
				String logMelding = String.format("; %s: Van oud bedrag (%s) naar nieuw bedrag %s", labTariefType.getNaam(),
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
			logMeldingBuilder.append(corrigeerOudeTarievenMelding).append(" nieuwe bedragen ").append(CervixTariefUtil.getTariefString(nieuwTarief));
		}

		return logMeldingBuilder.toString();

	}
}
