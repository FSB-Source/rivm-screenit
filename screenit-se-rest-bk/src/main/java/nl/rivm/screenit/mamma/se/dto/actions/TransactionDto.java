package nl.rivm.screenit.mamma.se.dto.actions;

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

import java.time.LocalDate;
import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;

public class TransactionDto
{
	private SETransactieType type;

	private Long clientId;

	private String sessionId;

	private String medewerkercode;

	private String uitnodigingsNr;

	@JsonDeserialize(using = LocalDateDeserializer.class)
	private LocalDate afspraakVanafDatum;

	private Long instellingGebruikerId;

	private List<ActionDto> actions;

	public LocalDate getAfspraakVanafDatum()
	{
		return afspraakVanafDatum;
	}

	public void setAfspraakVanafDatum(LocalDate afspraakVanafDatum)
	{
		this.afspraakVanafDatum = afspraakVanafDatum;
	}

	public Long getInstellingGebruikerId()
	{
		return instellingGebruikerId;
	}

	public void setInstellingGebruikerId(Long instellingGebruikerId)
	{
		this.instellingGebruikerId = instellingGebruikerId;
	}

	public String getMedewerkercode()
	{
		return medewerkercode;
	}

	public void setMedewerkercode(String medewerkercode)
	{
		this.medewerkercode = medewerkercode;
	}

	public String getUitnodigingsNr()
	{
		return uitnodigingsNr;
	}

	public void setUitnodigingsNr(String uitnodigingsNr)
	{
		this.uitnodigingsNr = uitnodigingsNr;
	}

	public SETransactieType getType()
	{
		return type;
	}

	public void setType(SETransactieType type)
	{
		this.type = type;
	}

	public List<ActionDto> getActions()
	{
		return actions;
	}

	public void setActions(List<ActionDto> actions)
	{
		this.actions = actions;
	}

	public Long getClientId()
	{
		return clientId;
	}

	public void setClientId(Long clientId)
	{
		this.clientId = clientId;
	}

	public String getSessionId()
	{
		return sessionId;
	}

	public void setSessionId(String sessionId)
	{
		this.sessionId = sessionId;
	}
}
