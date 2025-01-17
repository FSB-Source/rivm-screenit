package nl.rivm.screenit.mamma.se.proxy.util;

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

import java.io.IOException;
import java.time.LocalDate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class TransactionParser
{
	private static final ObjectMapper objectMapper = new ObjectMapper();

	private static final Logger LOG = LoggerFactory.getLogger(TransactionParser.class);

	private JsonNode transactionNode;

	public TransactionParser(String transactionJson)
	{
		try
		{
			transactionNode = objectMapper.readTree(transactionJson);
		}
		catch (IOException e)
		{
			LOG.error("Fout bij parsen van transactie DTO: {} ", transactionJson);
		}
	}

	public String getMedewerkercode()
	{
		return getStringOfPath("medewerkercode");
	}

	public String getUitnodigingsNr()
	{
		return getStringOfPath("uitnodigingsNr");
	}

	public String getTransactieType()
	{
		return getStringOfPath("type");
	}

	private String getStringOfPath(String path)
	{
		return transactionNode != null ? transactionNode.path(path).asText() : "onbekend";
	}

	public LocalDate getAfspraakVanafDatum()
	{
		if (transactionNode != null)
		{
			var afspraakVanafDatum = transactionNode.path("afspraakVanafDatum");
			return afspraakVanafDatum.isNull() ? DateUtil.getCurrentDate() : LocalDate.parse(afspraakVanafDatum.asText());
		}
		return DateUtil.getCurrentDate(); 
	}

	public String getAfspraakId()
	{
		var afspraakId = "onbekend";

		if (transactionNode != null)
		{
			var actions = transactionNode.path("actions");
			if (!actions.isEmpty())
			{
				return actions.get(0).path("afspraakId").toString();
			}
		}

		return afspraakId;
	}

	public Long getClientId()
	{
		return getLongOfPath("clientId");
	}

	public Long getInstellingGebruikerId()
	{
		return getLongOfPath("instellingGebruikerId");
	}

	private Long getLongOfPath(String path)
	{
		if (transactionNode != null)
		{
			var clientId = transactionNode.path(path).asLong(-1L);
			return clientId != -1 ? clientId : null;
		}
		return null;
	}
}
