package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-commons
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class AanvraagDto extends AbstractDtoReferenceObject
{
	private Date aanvraagDatum;

	private Integer aantal;

	private String status;

	private Date statusDatum;

	@NotNull(message = "Voor het aanvragen dient u een locatie te selecteren.")
	private LocatieDto locatie;

	private String aangevraagdDoor;

	public String getAangevraagdDoor()
	{
		return aangevraagdDoor;
	}

	public void setAangevraagdDoor(String aangevraagdDoor)
	{
		this.aangevraagdDoor = aangevraagdDoor;
	}

	public Date getAanvraagDatum()
	{
		return aanvraagDatum;
	}

	public void setAanvraagDatum(Date aanvraagDatum)
	{
		this.aanvraagDatum = aanvraagDatum;
	}

	public String getStatus()
	{
		return status;
	}

	public void setStatus(String status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public Integer getAantal()
	{
		return aantal;
	}

	public void setAantal(Integer aantal)
	{
		this.aantal = aantal;
	}

	public LocatieDto getLocatie()
	{
		return locatie;
	}

	public void setLocatie(LocatieDto locatie)
	{
		this.locatie = locatie;
	}
}
