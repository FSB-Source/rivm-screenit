package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.se.MammaKwaliteitsopnameDto;
import nl.rivm.screenit.mamma.se.dto.ErrorDto;
import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfrondenDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakMakenPassantDto;
import nl.rivm.screenit.mamma.se.dto.actions.AfspraakSignalerenDto;
import nl.rivm.screenit.mamma.se.dto.actions.DensiteitMetingActionDto;
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
import nl.rivm.screenit.mamma.se.service.MammografieService;
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
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.FoutmeldingsCodeUtil;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Slf4j
@RequiredArgsConstructor
public class SeTransactionServiceImpl implements SeTransactionService
{
	private final ObjectMapper objectMapper;

	private final InschrijvenService inschrijvenService;

	private final OnderzoekStartenService onderzoekStartenService;

	private final OnderzoekService onderzoekService;

	private final MammaBaseOnderzoekService baseOnderzoekService;

	private final MammaAfspraakService afspraakService;

	private final OnderzoekAfrondenService onderzoekAfrondenService;

	private final MammografieService mammografieService;

	private final MammaUitschrijvenService uitschrijvenService;

	private final SELogService logService;

	private final SeAutorisatieService seAutorisatieService;

	private final BerichtToBatchService hl7BerichtenToBatchService;

	private final InstellingService instellingService;

	private final ClientRepository clientRepository;

	@Override
	@Transactional
	public ResponseEntity executeAsTransactionIfAuthorised(List<ActionDto> acties, TransactionDto transactionDto, LocalDateTime transactieDatumTijd,
		InstellingGebruiker transactieGebruiker, MammaScreeningsEenheid screeningsEenheid) throws IOException
	{
		var client = transactionDto.getClientId() == null ? null : clientRepository.findById(transactionDto.getClientId()).orElseThrow();

		if (isAuthorized(transactieGebruiker, getRechtenVoorTransactie(acties)))
		{
			SEAccountResolverDelegate.setInstellingGebruiker(transactieGebruiker);
			for (var actionDto : acties)
			{
				executeAction(actionDto, transactieDatumTijd, transactieGebruiker, screeningsEenheid);
			}

			if (transactionDto.getType().equals(SETransactieType.LOG_GEBEURTENIS_SE))
			{
				logAlleLogActies(acties, transactieDatumTijd, transactieGebruiker, screeningsEenheid, client);
			}
			else if (!transactionDto.getType().equals(SETransactieType.BEEINDIGDE_AFSPRAAK_DOORVOEREN))
			{

				var onderzoek = client == null ? null : client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
				logTransaction(transactionDto, transactieDatumTijd, transactieGebruiker, screeningsEenheid, client, onderzoek);
			}
			return ResponseEntity.ok().build();
		}
		else
		{
			var referentie = FoutmeldingsCodeUtil.getFoutmeldingsCode("SE_REST");
			LOG.error("{}: Gebruiker is niet geautoriseerd om de actie uit te voeren", referentie);
			var errorDto = new ErrorDto(referentie);
			return ResponseEntity.status(HttpStatus.FORBIDDEN).body(errorDto);
		}
	}

	private void logTransaction(TransactionDto transactionDto, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker,
		MammaScreeningsEenheid screeningsEenheid, Client client, MammaOnderzoek onderzoek)
	{
		var logGebeurtenis = getLoggebeurtenis(transactionDto.getType(), onderzoek);
		if (logGebeurtenis != null)
		{
			logService.logInfo(logGebeurtenis, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
				getLogMessage(transactionDto.getType(), onderzoek));
		}
	}

	private void logAlleLogActies(List<ActionDto> acties, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker,
		MammaScreeningsEenheid screeningsEenheid, Client client) throws IOException
	{
		for (var dto : acties)
		{
			switch (dto.getType())
			{
			case LOG_GEBEURTENIS_BEELDEN_VORIGE_RONDE_OPGEHAALD:
				logService.logInfo(LogGebeurtenis.MAMMA_OUDE_BEELDEN_OPGEHAALD_OP_SE, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
					getActionBody(dto, LogBeeldenOpgehaaldActionDto.class).getLogMessage());
				break;
			case LOG_GEBEURTENIS_BEELDEN_GEEN_MPPS_ONTVANGEN:
				logService.logWarning(LogGebeurtenis.MAMMA_GEEN_MPPS_ONTVANGEN_SE, transactieGebruiker, client, screeningsEenheid.getId(), transactieDatumTijd,
					getActionBody(dto, LogGeenMppsBerichtActionDto.class).getLogMessage());
				break;
			case LOG_GEBEURTENIS_BEELDEN_ANNOTATIE_AMPUTATIE_MISMATCH:
				logService.logInfo(LogGebeurtenis.MAMMA_VERSCHIL_BEELDEN_ANNOTATIE_AMPUTATIE_SE, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
					getActionBody(dto, LogAmputatieBeeldenAnnotatieMismatchActionDto.class).getLogMessage());
				break;
			case LOG_GEBEURTENIS_GEFAALDE_TRANSACTIE:
				List<Instelling> instellingen = Collections.singletonList(instellingService.getActieveInstellingen(Rivm.class).get(0));
				logService.logError(LogGebeurtenis.MAMMA_GEFAALDE_TRANSACTIE_SE, transactieGebruiker, client, screeningsEenheid, instellingen, transactieDatumTijd,
					getActionBody(dto, LogGefaaldeTransactieActionDto.class).getLogMessage());
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
		case STRUCTURED_REPORT_ONTVANGEN:
			return null;
		default:
			throw new IllegalArgumentException("Transactietype voor loggebeurtenis niet gevonden voor " + transactieType);
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
			return signalerenOpslaanLogMessage(onderzoek);
		default:
			throw new IllegalStateException("Transactietype niet gevonden");
		}
	}

	private String signalerenOpslaanLogMessage(MammaOnderzoek onderzoek)
	{
		if (onderzoek.getSignaleren() != null)
		{
			return onderzoek.getSignaleren().isHeeftAfwijkingen() ? "Er zijn afwijkingen gesignaleerd." : "";
		}
		return "";
	}

	private void executeAction(ActionDto actionDto, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker, MammaScreeningsEenheid screeningsEenheid)
		throws IOException
	{
		switch (actionDto.getType())
		{
		case INSCHRIJVEN:
			inschrijvenService.inschrijven(getActionBody(actionDto, InschrijvenDto.class), transactieGebruiker, transactieDatumTijd, screeningsEenheid);
			break;
		case CLIENTGEGEVENS_OPSLAAN:
			inschrijvenService.inschrijvingWijzigen(getActionBody(actionDto, InschrijvenDto.class), transactieGebruiker, transactieDatumTijd,
				screeningsEenheid);
			break;
		case UITSCHRIJVEN:
			uitschrijvenService.uitschrijven(getActionBody(actionDto, UitschrijvenDto.class), transactieGebruiker);
			break;
		case SET_EMAILADRES:
			inschrijvenService.setEmailAdres(getActionBody(actionDto, SetEmailAdresDto.class), transactieGebruiker);
			break;
		case ONDERZOEK_STARTEN:
			onderzoekStartenService.starten(getActionBody(actionDto, OnderzoekStartenDto.class), screeningsEenheid, transactieDatumTijd,
				transactieGebruiker);
			break;
		case MAMMOGRAFIE_OPSLAAN_EN_STATUSOVERGANG:
			mammografieService.opslaanEnStatusovergang(getActionBody(actionDto, MammografieOpslaanDto.class), transactieGebruiker,
				transactieDatumTijd);
			break;
		case MAMMOGRAFIE_OPSLAAN:
			mammografieService.opslaan(getActionBody(actionDto, MammografieOpslaanDto.class), transactieGebruiker);
			break;
		case DENSITEITMETING:
			mammografieService.densiteitMetingOpslaan(getActionBody(actionDto, DensiteitMetingActionDto.class));
			break;
		case ONDERZOEK_OPSLAAN:
			onderzoekService.opslaan(getActionBody(actionDto, OnderzoekOpslaanDto.class), transactieGebruiker);
			break;
		case MAAK_DUBBELE_TIJD:
			onderzoekService.maakDubbeleTijd(getActionBody(actionDto, MaakDubbeleTijdDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case MAAK_DUBBELE_TIJD_REDEN:
			onderzoekService.maakDubbeleTijdReden(getActionBody(actionDto, MaakDubbeleTijdRedenDto.class), transactieGebruiker);
			break;
		case AFSPRAAK_SIGNALEREN:
			afspraakService.setAfspraakStatus(getActionBody(actionDto, AfspraakSignalerenDto.class), MammaAfspraakStatus.SIGNALEREN, transactieGebruiker);
			break;
		case AFSPRAAK_AFRONDEN:
			onderzoekAfrondenService.beeindigen(getActionBody(actionDto, AfrondenDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case AFSPRAAK_SIGNALEREN_OPSLAAN:
			onderzoekService.signalerenOpslaan(getActionBody(actionDto, SignalerenOpslaanDto.class), transactieGebruiker, transactieDatumTijd);
			break;
		case ONDERZOEK_AFRONDEN:
			onderzoekAfrondenService.onderzoekAfronden(getActionBody(actionDto, OnderzoekAfrondenDto.class), transactieGebruiker, transactieDatumTijd);
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
			afspraakService.afspraakMakenPassant(getActionBody(actionDto, AfspraakMakenPassantDto.class), transactieGebruiker, screeningsEenheid);
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

	private <T> T getActionBody(ActionDto actionDto, Class<T> valueType) throws JsonProcessingException
	{
		return objectMapper.readValue(actionDto.getNodeText(), valueType);
	}

	private void afspraakDoorvoeren(ActionDto actionDto, LocalDateTime transactieDatumTijd, InstellingGebruiker transactieGebruiker, MammaScreeningsEenheid screeningsEenheid)
		throws IOException
	{
		var afspraak = afspraakService
			.getOfMaakLaatsteAfspraakVanVandaag(getActionBody(actionDto, OnderzoekDoorvoerenDto.class).getAfspraakId(), transactieGebruiker);
		var onderzoek = afspraak.getOnderzoek();
		if (onderzoek.isDoorgevoerd())
		{
			LOG.info("Onderzoek {} met status {} is al doorgevoerd; negeer actie {}", onderzoek.getId(), onderzoek.getStatus(), actionDto.getType());
		}
		else
		{
			baseOnderzoekService.onderzoekDoorvoerenVanuitSe(onderzoek);
			var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
			logService.logInfo(LogGebeurtenis.MAMMA_SE_ONDERZOEK_DOORGEVOERD, transactieGebruiker, client, screeningsEenheid, transactieDatumTijd,
				getLogMessage(SETransactieType.BEEINDIGDE_AFSPRAAK_DOORVOEREN, onderzoek));

		}
	}

	private void kwaliteitsopname(ActionDto actionDto, MammaHL7v24ORMBerichtStatus ormBerichtStatus) throws IOException
	{
		var kwaliteitsopname = getActionBody(actionDto, MammaKwaliteitsopnameDto.class);
		LOG.info("Kwaliteitsopname {}, {}", ormBerichtStatus, ToStringBuilder.reflectionToString(kwaliteitsopname));
		hl7BerichtenToBatchService.queueMammaKwaliteitsopnameHL7v24BerichtUitgaand(kwaliteitsopname, ormBerichtStatus);
	}

	private List<Recht> getRechtenVoorTransactie(List<ActionDto> acties)
	{
		return acties.stream()
			.map(a -> a.getType().getRecht())
			.filter(Objects::nonNull)
			.distinct()
			.collect(Collectors.toList());
	}

	private boolean isAuthorized(InstellingGebruiker instellingGebruiker, List<Recht> rechten)
	{
		return rechten.stream().allMatch(r -> seAutorisatieService.isInstellingGebruikerGeautoriseerd(instellingGebruiker, r));
	}
}
