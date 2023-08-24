package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class PassantSeDto
{

	private ClientSeDto clientSeDto;

	private LocalDateTime afspraakVanaf;

	private String afspraakSe;

	private LocalDateTime uitnodigingsDatum;

	private boolean eenmaligeAfmelding;

	public PassantSeDto(ClientSeDto clientSeDto, LocalDateTime afspraakVanaf, String afspraakSe, LocalDateTime uitnodigingsDatum, boolean eenmaligeAfmelding)
	{
		this.clientSeDto = clientSeDto;
		this.afspraakVanaf = afspraakVanaf;
		this.afspraakSe = afspraakSe;
		this.uitnodigingsDatum = uitnodigingsDatum;
		this.eenmaligeAfmelding = eenmaligeAfmelding;
	}

	public ClientSeDto getClientSeDto()
	{
		return clientSeDto;
	}

	public void setClientSeDto(ClientSeDto clientSeDto)
	{
		this.clientSeDto = clientSeDto;
	}

	public LocalDateTime getAfspraakVanaf()
	{
		return afspraakVanaf;
	}

	public void setAfspraakVanaf(LocalDateTime afspraakVanaf)
	{
		this.afspraakVanaf = afspraakVanaf;
	}

	public String getAfspraakSe()
	{
		return afspraakSe;
	}

	public void setAfspraakSe(String afspraakSe)
	{
		this.afspraakSe = afspraakSe;
	}

	public LocalDateTime getUitnodigingsDatum()
	{
		return uitnodigingsDatum;
	}

	public void setUitnodigingsDatum(LocalDateTime uitnodigingsDatum)
	{
		this.uitnodigingsDatum = uitnodigingsDatum;
	}

	public Boolean getEenmaligeAfmelding()
	{
		return eenmaligeAfmelding;
	}

	public void setEenmaligeAfmelding(Boolean eenmaligeAfmelding)
	{
		this.eenmaligeAfmelding = eenmaligeAfmelding;
	}
}
