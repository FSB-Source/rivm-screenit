package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.hibernate.validator.constraints.NotEmpty;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
public class WachtwoordWijzigenDto
{

	private String oudeWachtwoord;

	@NotEmpty(message = "Wachtwoord is verplicht.")
	private String nieuweWachtwoord;

	@NotEmpty(message = "Wachtwoord controle is verplicht.")
	private String nieuweWachtwoordControle;

	public String getOudeWachtwoord()
	{
		return oudeWachtwoord;
	}

	public void setOudeWachtwoord(String oudeWachtwoord)
	{
		this.oudeWachtwoord = oudeWachtwoord;
	}

	public String getNieuweWachtwoord()
	{
		return nieuweWachtwoord;
	}

	public void setNieuweWachtwoord(String nieuweWachtwoord)
	{
		this.nieuweWachtwoord = nieuweWachtwoord;
	}

	public String getNieuweWachtwoordControle()
	{
		return nieuweWachtwoordControle;
	}

	public void setNieuweWachtwoordControle(String nieuweWachtwoordControle)
	{
		this.nieuweWachtwoordControle = nieuweWachtwoordControle;
	}
}
