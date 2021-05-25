package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.se.MammaKwaliteitsopnameDto;
import nl.rivm.screenit.mamma.se.dto.ErrorDto;
import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfrondenDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakMakenPassantDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakSignalerenDto;
import nl.rivm.screenit.mamma.se.dto.actions.InschrijvenDto;
import nl.rivm.screenit.mamma.se.dto.actions.LogAmputatieBeeldenAnnotatieMismatchActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.LogBeeldenOpgehaaldActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.LogGeenMppsBerichtActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.LogGefaaldeTransactieActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdRedenDto;
import nl.rivm.screenit.mamma.se.dto.actions.MammografieOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekAfrondenDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekDoorvoerenDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekStartenDto;
import nl.rivm.screenit.mamma.se.dto.actions.SETransactieType;
import nl.rivm.screenit.mamma.se.dto.actions.SetEmailAdresDto;
import nl.rivm.screenit.mamma.se.dto.actions.SignalerenOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.actions.TransactionDto;
import nl.rivm.screenit.mamma.se.dto.actions.UitschrijvenDto;
import nl.rivm.screenit.mamma.se.security.SEAccountResolverDelegate;
import nl.rivm.screenit.mamma.se.service.InschrijvenService;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.MammaUitschrijvenService;
import nl.rivm.screenit.mamma.se.service.MammografieAanmakenService;
import nl.rivm.screenit.mamma.se.service.OnderzoekAfrondenService;
import nl.rivm.screenit.mamma.se.service.OnderzoekService;
import nl.rivm.screenit.mamma.se.service.OnderzoekStartenService;
import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.mamma.se.service.SeAutorisatieService;
import nl.rivm.screenit.mamma.se.service.SeTransactionService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.FoutmeldingsCodeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class SeTransactionServiceImpl implements SeTransactionService
{
	private static final Logger LOG = LoggerFactory.getLogger(SeTransactionService.class);

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private InschrijvenService inschrijvenService;

	@Autowired
	private OnderzoekStartenService onderzoekStartenService;

	@Autowired
	private OnderzoekService onderzoekService;

	@Autowired
	private MammaBaseOnderzoekService baseOnderzoekService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Autowired
	private OnderzoekAfrondenService onderzoekAfrondenService;

	@Autowired
	private MammografieAanmakenService mammografieAanmakenService;

	@Autowired
	private MammaUitschrijvenService uitschrijvenService;

	@Autowired
	private SELogService logService;

	@Autowired
	private SeAutorisatieService seAutorisatieService;

	@Autowired
	private BerichtToBatchService hl7BerichtenToBatchService;

	@Autowired
	private InstellingService instellingService;

	@Override
	public ResponseEntity executeAsTransactionIfAuthorised(List<ActionDto> acties, TransactionDto transactionDto, LocalDateTime transactieDatumTijd,
		InstellingGebruiker transactieGebruiker, MammaScreeningsEenheid screeningsEenheid) throws IOException
	{
		Client client = transactionDto.getClientId() == null ? null : hibernateService.load(Client.class, transactionDto.getClientId());

		if (isAuthorized(transactieGebruiker, getRechtenVoorTransactie(acties)))
		{
			SEAccountResolverDelegate.setInstellingGebruiker(transactieGebruiker);
			for (ActionDto actionDto : acties)
			{
				executeAction(actionDto, transactieDatumTijd, transactieGebruiker, screeningsEenheid);
			}

			if (transactionDto.getType().equals(SETransactieType.LOG_GEBEURTENIS_SE))
			{
				logAlleLogActies(acties, transactieDatumTijd, transactieGebruiker, screeningsEenheid, client);
			}
			else if (!transactionDto.getType().equals(SETransactieType.BEEINDIGDE_AFSPRAAK_DOORVOEREN))
			{

				MammaOnderzoek onderzoek = client == null ? null : client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
				logTransaction(transactionDto, transactieDatumTijd, transactieGebruiker, screeningsEenheid, client, onderzoek);
			}
			return ResponseEntity.ok().build();
		}
		else
		{
			String referentie = FoutmeldingsCodeUtil.getFoutmeldingsCode("SE_REST");
			LOG.error(referentie + ": Gebruiker is niet geautoriseerd om de actie uit te voeren");
			ErrorDto errorDto = new ErrorDto(referentie);
			return ResponseEntity.status(HttpStatus.FORBIDDEN).body(errorDto);
		}
	}

	private void logTransaction(TransactionDto transactionDto, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker,
		MammaScreeningsEenheid screeningsEenheid, Client client, MammaOnderzoek onderzoek)
	{
		LogGebeurtenis logGebeurtenis = getLoggebeurtenis(transactionDto.getType(), onderzoek);
		if (logGebeurtenis != null)
		{
			logService.logInfo(logGebeurtenis, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
				getLogMessage(transactionDto.getType(), onderzoek));
		}
	}

	private void logAlleLogActies(List<ActionDto> acties, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker,
		MammaScreeningsEenheid screeningsEenheid, Client client) throws IOException
	{
		for (ActionDto dto : acties)
		{
			switch (dto.getType())
			{
			case LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD:
				logService.logInfo(LogGebeurtenis.MAMMA_OUDE_BEELDEN_OPGEHAALD_OP_SE, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
					objectMapper.readValue(dto.getNodeText(), LogBeeldenOpgehaaldActionDto.class).getLogMessage());
				break;
			case LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN:
				logService.logWarning(LogGebeurtenis.MAMMA_GEEN_MPPS_ONTVANGEN_SE, transactieGebruiker, client, screeningsEenheid.getId(), transactieDatumTijd,
					objectMapper.readValue(dto.getNodeText(), LogGeenMppsBerichtActionDto.class).getLogMessage());
				break;
			case LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH:
				logService.logInfo(LogGebeurtenis.MAMMA_VERSCHIL_BEELDEN_ANNOTATIE_AMPUTATIE_SE, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
					objectMapper.readValue(dto.getNodeText(), LogAmputatieBeeldenAnnotatieMismatchActionDto.class).getLogMessage());
				break;
			case LOG_GEBEURTENIS_GEFAALDE_TRANSACTIE:
				List<Instelling> instellingen = Collections.singletonList(instellingService.getActieveInstellingen(Rivm.class).get(0));
				logService.logError(LogGebeurtenis.MAMMA_GEFAALDE_TRANSACTIE_SE, transactieGebruiker, client, screeningsEenheid, instellingen, transactieDatumTijd,
					objectMapper.readValue(dto.getNodeText(), LogGefaaldeTransactieActionDto.class).getLogMessage());
				break;
			default:
				throw new IllegalStateException("Onbekende log actie in de SE Transaction Service");
			}
		}
	}

	private LogGebeurtenis getLoggebeurtenis(SETransactieType transactieType, MammaOnderzoek onderzoek)
	{
		switch (transactieType)
		{
		case INSCHRIJFGEGEVENS_OPSLAAN:
			return LogGebeurtenis.MAMMA_SE_INSCHRIJVEN;
		case ONDERZOEK_STARTEN:
			return LogGebeurtenis.MAMMA_SE_ONDERZOEK_STARTEN;
		case VISUELE_INSPECTIE_OPSLAAN:
			return LogGebeurtenis.MAMMA_SE_ONDERZOEK_UITGEVOERD;
		case SIGNALEREN_OPSLAAN:
			if (onderzoek.isDoorgevoerd())
			{
				throw new IllegalStateException("Onderzoek met id [" + onderzoek.getId() + "] is klaar voor beoordeling en mag niet meer worden gewijzigd.");
			}
			switch (onderzoek.getStatus())
			{
			case ONVOLLEDIG:
				return LogGebeurtenis.MAMMA_SE_ONDERZOEK_ONVOLLEDIG;
			case ONDERBROKEN:
				return LogGebeurtenis.MAMMA_SE_ONDERZOEK_ONDERBROKEN;
			case AFGEROND:
				return LogGebeurtenis.MAMMA_SE_ONDERZOEK_AFGEROND;
			default:
				return null;
			}
		case INSCHRIJVEN_PASSANT:
			return LogGebeurtenis.MAMMA_SE_INSCHRIJVEN_PASSANT;
		case UITSCHRIJVEN_CLIENT:
			return LogGebeurtenis.MAMMA_SE_UITSCHRIJVEN;
		case START_KWALITEITSOPNAME_TRANSACTION:
			return LogGebeurtenis.MAMMA_SE_START_KWALITEITSOPNAME;
		case BEEINDIG_KWALITEITSOPNAME_TRANSACTION:
			return LogGebeurtenis.MAMMA_SE_BEEINDIG_KWALITEITSOPNAME;
		default:
			throw new IllegalStateException("Transactietype niet gevonden");
		}
	}

	private String getLogMessage(SETransactieType transactieType, MammaOnderzoek onderzoek)
	{
		switch (transactieType)
		{
		case INSCHRIJFGEGEVENS_OPSLAAN:
		case ONDERZOEK_STARTEN:
		case VISUELE_INSPECTIE_OPSLAAN:
		case INSCHRIJVEN_PASSANT:
		case UITSCHRIJVEN_CLIENT:
		case START_KWALITEITSOPNAME_TRANSACTION:
		case BEEINDIG_KWALITEITSOPNAME_TRANSACTION:
		case BEEINDIGDE_AFSPRAAK_DOORVOEREN:
		case LOG_GEBEURTENIS_SE:
			return "";
		case SIGNALEREN_OPSLAAN:
			return (onderzoek.getSignaleren() != null) ? onderzoek.getSignaleren().getHeeftAfwijkingen() ? "Er zijn afwijkingen gesignaleerd." : "" : "";
		default:
			throw new IllegalStateException("Transactietype niet gevonden");
		}
	}

	private void executeAction(ActionDto actionDto, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker, MammaScreeningsEenheid screeningsEenheid)
		throws IOException
	{
		switch (actionDto.getType())
		{
		case INSCHRIJVEN:
			inschrijvenService.inschrijven(objectMapper.readValue(actionDto.getNodeText(), InschrijvenDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case CLIENTGEGEVENS_OPSLAAN:
			inschrijvenService.inschrijvingWijzigen(objectMapper.readValue(actionDto.getNodeText(), InschrijvenDto.class), transactieGebruiker);
			break;
		case UITSCHRIJVEN:
			uitschrijvenService.uitschrijven(objectMapper.readValue(actionDto.getNodeText(), UitschrijvenDto.class), transactieGebruiker);
			break;
		case SET_EMAILADRES:
			inschrijvenService.setEmailAdres(objectMapper.readValue(actionDto.getNodeText(), SetEmailAdresDto.class), transactieGebruiker);
			break;
		case ONDERZOEK_STARTEN:
			onderzoekStartenService.starten(objectMapper.readValue(actionDto.getNodeText(), OnderzoekStartenDto.class), screeningsEenheid, transactieDatumTijd,
				transactieGebruiker);
			break;
		case MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG:
			mammografieAanmakenService.opslaanEnStatusovergang(objectMapper.readValue(actionDto.getNodeText(), MammografieOpslaanDto.class), transactieGebruiker,
				transactieDatumTijd);
			break;
		case MAMMOGRAFIE_OPSLAAN:
			mammografieAanmakenService.opslaan(objectMapper.readValue(actionDto.getNodeText(), MammografieOpslaanDto.class), transactieGebruiker);
			break;
		case ONDERZOEK_OPSLAAN:
			onderzoekService.opslaan(objectMapper.readValue(actionDto.getNodeText(), OnderzoekOpslaanDto.class), transactieGebruiker);
			break;
		case MAAK_DUBBELE_TIJD:
			onderzoekService.maakDubbeleTijd(objectMapper.readValue(actionDto.getNodeText(), MaakDubbeleTijdDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case MAAK_DUBBELE_TIJD_REDEN:
			onderzoekService.maakDubbeleTijdReden(objectMapper.readValue(actionDto.getNodeText(), MaakDubbeleTijdRedenDto.class), transactieGebruiker);
			break;
		case AFSPRAAK_SIGNALEREN:
			afspraakService.setAfspraakStatus(objectMapper.readValue(actionDto.getNodeText(), AfspraakSignalerenDto.class), MammaAfspraakStatus.SIGNALEREN, transactieGebruiker);
			break;
		case AFSPRAAK_AFRONDEN:
			onderzoekAfrondenService.beeindigen(objectMapper.readValue(actionDto.getNodeText(), AfrondenDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case AFSPRAAK_SIGNALEREN_OPSLAAN:
			onderzoekService.signalerenOpslaan(objectMapper.readValue(actionDto.getNodeText(), SignalerenOpslaanDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case ONDERZOEK_AFRONDEN:
			onderzoekAfrondenService.onderzoekAfronden(objectMapper.readValue(actionDto.getNodeText(), OnderzoekAfrondenDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD:
		case LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH:
		case LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN:
		case LOG_GEBEURTENIS_GEFAALDE_TRANSACTIE:
			break;
		case AFSPRAAK_DOORVOEREN:
			afspraakDoorvoeren(actionDto, transactieDatumTijd, transactieGebruiker, screeningsEenheid);
			break;
		case AFSPRAAK_MAKEN_PASSANT:
			afspraakService.afspraakMakenPassant(objectMapper.readValue(actionDto.getNodeText(), AfspraakMakenPassantDto.class), transactieGebruiker, screeningsEenheid);
			break;
		case START_KWALITEITSOPNAME:
			kwaliteitsopname(actionDto, MammaHL7v24ORMBerichtStatus.STARTED);
			break;
		case BEEINDIG_KWALITEITSOPNAME:
			kwaliteitsopname(actionDto, MammaHL7v24ORMBerichtStatus.COMPLETED);
			break;
		case UPDATE_HEEFT_AFWIJKINGEN:
		case MAAK_SIGNALERING_ICOON_RECHTS_HORIZONTAAL:
		case MAAK_SIGNALERING_ICOON_LINKS_HORIZONTAAL:
		case MAAK_SIGNALERING_ICOON_RECHTS_VERTICAAL:
		case MAAK_SIGNALERING_ICOON_LINKS_VERTICAAL:
		case SET_SIGNALERING:
		case SET_VISUELE_INSPECTIE_AFBEELDING:
		case WIJZIGINGEN_GEMAAKT:

			break;

		default:
			throw new IllegalArgumentException("Onbekende actie: " + actionDto.getType());
		}
	}

	private void afspraakDoorvoeren(ActionDto actionDto, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker, MammaScreeningsEenheid screeningsEenheid)
		throws IOException
	{
		MammaAfspraak afspraak = afspraakService
			.getOfMaakLaatsteAfspraakVanVandaag(objectMapper.readValue(actionDto.getNodeText(), OnderzoekDoorvoerenDto.class).getAfspraakId(), transactieGebruiker);
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		if (onderzoek.isDoorgevoerd())
		{
			LOG.info("Onderzoek {} met status {} is al doorgevoerd; negeer actie {}", onderzoek.getId(), onderzoek.getStatus(), actionDto.getType());
		}
		else
		{
			baseOnderzoekService.onderzoekDoorvoerenVanuitSe(onderzoek);
			Client client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
			logService.logInfo(LogGebeurtenis.MAMMA_SE_ONDERZOEK_DOORGEVOERD, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
				getLogMessage(SETransactieType.BEEINDIGDE_AFSPRAAK_DOORVOEREN, onderzoek));

		}
	}

	private void kwaliteitsopname(ActionDto actionDto, MammaHL7v24ORMBerichtStatus ormBerichtStatus) throws IOException
	{
		MammaKwaliteitsopnameDto kwaliteitsopname = objectMapper.readValue(actionDto.getNodeText(), MammaKwaliteitsopnameDto.class);
		LOG.info("Kwaliteitsopname " + ormBerichtStatus + ", " + ToStringBuilder.reflectionToString(kwaliteitsopname));
		hl7BerichtenToBatchService.queueMammaKwaliteitsopnameHL7v24BerichtUitgaand(kwaliteitsopname, ormBerichtStatus);
	}

	private List<Recht> getRechtenVoorTransactie(List<ActionDto> acties)
	{
		return acties.stream().map(a -> a.getType().getRecht()).collect(Collectors.toList());
	}

	private boolean isAuthorized(InstellingGebruiker instellingGebruiker, List<Recht> rechten)
	{
		return rechten.stream().allMatch(r -> seAutorisatieService.isInstellingGebruikerGeautoriseerd(instellingGebruiker, r));
	}
}
