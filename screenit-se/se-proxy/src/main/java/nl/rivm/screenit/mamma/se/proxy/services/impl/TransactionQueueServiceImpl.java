package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.model.PersistableTransaction;
import nl.rivm.screenit.mamma.se.proxy.services.PersistableTransactionService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;
import nl.rivm.screenit.mamma.se.proxy.util.TransactionParser;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class TransactionQueueServiceImpl implements TransactionQueueService, ApplicationContextAware
{
	@Autowired
	private PersistableTransactionService persistableTransactionService;

	private static TransactionQueueVerwerker transactionQueueVerwerker;

	private ApplicationContext applicationContext;

	@Getter
	private final Object daglijstEnTransactieLock = new Object();

	@Override
	public synchronized void ensureRunningQueueVerwerking()
	{
		if (transactionQueueVerwerker == null || !transactionQueueVerwerker.isAlive())
		{
			transactionQueueVerwerker = applicationContext.getBean(TransactionQueueVerwerker.class);
			transactionQueueVerwerker.start();
		}
	}

	@Override
	public void addTransactionToQueue(String transactionJSON, Long clientId)
	{
		final PersistableTransaction transaction = new PersistableTransaction(DateUtil.getCurrentDateTime(), transactionJSON, clientId);
		persistableTransactionService.putLast(transaction);
		LOG.debug("Transactie toegevoegd aan queue: " + transactieLogTekst(transaction));
		ensureRunningQueueVerwerking();
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

	@Override
	public List<String> getPendingTransactionDtos(LocalDate datum)
	{
		List<String> result;
		result = persistableTransactionService.getAll().stream()
			.map(PersistableTransaction::getTransactie)
			.filter(tx -> new TransactionParser(tx).getAfspraakVanafDatum().equals(datum))
			.collect(Collectors.toList());
		return result;
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException
	{
		this.applicationContext = applicationContext;
	}
}
