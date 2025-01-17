package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;

public class PersistableTransaction
{
	private Long transactionId;

	private final LocalDateTime datumTijd;

	private final String transactie;

	private final Long clientId;

	public PersistableTransaction(LocalDateTime datumTijd, String transactie, Long clientId)
	{
		this.datumTijd = datumTijd;
		this.transactie = transactie;
		this.clientId = clientId;
	}

	public PersistableTransaction(long id, LocalDateTime datumTijd, String transactie, Long clientId)
	{
		this(datumTijd, transactie, clientId);
		this.transactionId = id;
	}

	public Long getTransactionId()
	{
		return transactionId;
	}

	public LocalDateTime getDatumTijd()
	{
		return datumTijd;
	}

	public String getTransactie()
	{
		return transactie;
	}

	public Long getClientId()
	{
		return clientId;
	}
}
