package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collections;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.model.LogGefaaldeTransactieGebeurtenisAction;
import nl.rivm.screenit.mamma.se.proxy.model.PersistableTransaction;
import nl.rivm.screenit.mamma.se.proxy.model.SETransactieType;
import nl.rivm.screenit.mamma.se.proxy.model.TransactieDto;
import nl.rivm.screenit.mamma.se.proxy.services.PersistableTransactionService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;
import nl.rivm.screenit.mamma.se.proxy.util.TransactionParser;
import nl.rivm.screenit.mamma.se.proxy.util.TransactionSerializer;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@Scope("prototype")
public class TransactionQueueVerwerker extends Thread
{
	@Autowired
	private SeDaglijstService daglijstService;

	@Autowired
	private TransactionQueueService transactionQueueService;

	@Autowired
	private PersistableTransactionService persistableTransactionService;

	@Autowired
	private SeRestSocketService restSocketService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private SeStatusService statusService;

	@Override
	public void run()
	{
		HttpStatus responseStatus = null;
		while (!HttpStatus.GATEWAY_TIMEOUT.equals(responseStatus) && statusService.isOnline() && persistableTransactionService.zijnErWachtendeTransacties())
		{
			PersistableTransaction transaction = persistableTransactionService.takeFirst();
			if (transaction != null)
			{
				synchronized (transactionQueueService.getDaglijstEnTransactieLock())
				{
					responseStatus = executeTransaction(transaction);
					if (responseStatus.equals(HttpStatus.GATEWAY_TIMEOUT))
					{
						restSocketService.closeSocket();
					}
					else
					{
						String transactionJSON = transaction.getTransactie();

						if (responseStatus.equals(HttpStatus.OK) || transaction.getClientId() == 0)
						{
							LocalDate afspraakDatum = new TransactionParser(transactionJSON).getAfspraakVanafDatum();
							daglijstService.voegVerwerkteTransactionDtoToe(afspraakDatum, transactionJSON);
							toevoegenAanVerstuurdeTransacties(transaction, transactionJSON);
							persistableTransactionService.remove(transaction.getTransactionId());
						}
					}
				}
			}
		}
	}

	private void toevoegenAanVerstuurdeTransacties(PersistableTransaction transaction, String transactionJSON)
	{
		if (!transactionJSON.contains("INSCHRIJVEN")
			&& !transactionJSON.contains("CLIENTGEGEVENS_OPSLAAN")
			&& !transactionJSON.contains("SET_EMAILADRES")
			&& !transactionJSON.contains("AFSPRAAK_MAKEN_PASSANT"))
		{
			persistableTransactionService.addToVerstuurdeTransacties(transaction);
		}
	}

	private RequestEntity maakTransactionRequest(PersistableTransaction transaction)
	{
		RequestEntity.BodyBuilder requestBuilder = proxyService.getProxyRequestEntity("/transaction", HttpMethod.PUT);
		requestBuilder.header("TRANSACTIE_DATUMTIJD", DateTimeFormatter.ISO_DATE_TIME.format(transaction.getDatumTijd()));
		return requestBuilder.body(transaction.getTransactie());
	}

	private HttpStatus executeTransaction(PersistableTransaction transaction)
	{
		String transactieJson = transaction.getTransactie();

		ResponseEntity<String> result = proxyService.sendUncheckedProxyRequest(maakTransactionRequest(transaction), String.class);

		if (result.getStatusCode().equals(HttpStatus.OK))
		{
			LOG.info("Transactie succesvol verwerkt: " + transactieLogTekst(transaction));
		}
		else if (result.getStatusCode().equals(HttpStatus.GATEWAY_TIMEOUT))
		{
			LOG.info("Transactie niet verwerkt, SE lijkt offline: " + transactieLogTekst(transaction));
		}
		else
		{
			LOG.error("Transactie niet verwerkt, fout bij uitvoeren. Nieuwe daglijst wordt opgehaald: " + transactieLogTekst(transaction));
			if (transaction.getClientId() != 0)
			{
				persistableTransactionService.addToFouteTransactie(transaction);
				addGefaaldeTransactieLogToQueue(transaction);
			}
			daglijstService.queueDaglijstOphalenVanDag(new TransactionParser(transactieJson).getAfspraakVanafDatum());
		}

		return result.getStatusCode();
	}

	private String transactieLogTekst(PersistableTransaction transaction)
	{
		TransactionParser transactionParser = new TransactionParser(transaction.getTransactie());

		String logTekst = String.format("[transactieType: %s] [transactieTijd: %s] [clientId: %s] [uitnodigingsNr: %s] [medewerkercode: %s]",
			transactionParser.getTransactieType(), transaction.getDatumTijd(), transaction.getClientId(),
			transactionParser.getUitnodigingsNr(), transactionParser.getMedewerkercode());

		if (transaction.getTransactionId() != null)
		{
			return String.format("%s, [transactieId: %s]", logTekst, transaction.getTransactionId());
		}
		return logTekst;
	}

	private void addGefaaldeTransactieLogToQueue(PersistableTransaction transactie)
	{
		TransactieDto transactieDto = new TransactieDto(
			SETransactieType.LOG_GEBEURTENIS_SE,
			transactie.getClientId(),
			new TransactionParser(transactie.getTransactie()).getInstellingGebruikerId(),
			Collections.singletonList(new LogGefaaldeTransactieGebeurtenisAction(statusService.getSeCode())),
			DateUtil.getCurrentDateTime().toLocalDate());

		String json = TransactionSerializer.writeAsString(transactieDto);
		transactionQueueService.addTransactionToQueue(json, null);
	}

}
