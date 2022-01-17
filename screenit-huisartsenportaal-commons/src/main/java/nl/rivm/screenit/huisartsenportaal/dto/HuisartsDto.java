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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
public class HuisartsDto extends AbstractDtoReferenceObject
{
	@NotNull(message = "AGB-code is verplicht.")
	@Size(min = 8)
	private String agbcode;

	@NotNull(message = "E-mail is verplicht.")
	@Pattern(regexp = "^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*((\\.[A-Za-z]{2,}){1}$)", message = "E-mail voldoet niet aan het verplichte formaat")
	private String email;

	private String aanhef;

	@NotNull(message = "Achternaam is verplicht.")
	@Size(max = 100)
	@Pattern(regexp = "^[a-zA-Z\\u00C0-\\u024F\\-'\\s]*$", message = "Achternaam mag alleen letters, spaties en koppeltekens bevatten")
	private String achternaam;

	@Size(max = 20, message = "Tussenvoegsel mag maximaal 20 tekens bevatten")
	@Pattern(regexp = "^[a-zA-Z\\u00C0-\\u024F'\\s]*$", message = "Tussenvoegsel mag alleen letters en spaties bevatten")
	private String tussenvoegsel;

	@Size(max = 20, message = "'Voorletters' mag maximaal 20 tekens bevatten")
	@Pattern(regexp = "^[a-zA-Z\\u00C0-\\u024F.]*$", message = "'Voorletters' mag alleen letters en punten bevatten")
	private String voorletters;

	@Size(max = 25, message = "Telefoonnummer mag maximaal 25 tekens bevatten")
	private String telefoon;

	@JsonIgnore
	private String inlogCode;

	@JsonIgnore
	private String aanmeldStatus;

	@JsonIgnore
	private String status;

	private String username;

	private String wachtwoord;

	@JsonIgnore
	private Boolean actief;

	@NotNull(message = "U dient de zakelijke voorwaarden te accorderen.")
	private Boolean overeenkomst;

	@Pattern(
		regexp = "^(([a-zA-Z0-9_\\-\\.]+)@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.)|(([a-zA-Z0-9\\-]+\\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\\]?)(\\s*(;|,)\\s*|\\s*$))*$",
		message = "Eén of meerdere van de e-mailadressen voldoet niet aan het verplichte formaat")
	private String extraEmails;

	private Date overeenkomstGeaccordeerdDatum;

	@Valid
	@NotNull
	private AdresDto postadres = new AdresDto();

	@Valid
	@NotNull
	private List<LocatieDto> locaties = new ArrayList<LocatieDto>();

	public String getAgbcode()
	{
		return agbcode;
	}

	public void setAgbcode(String agbcode)
	{
		this.agbcode = agbcode;
	}

	public String getEmail()
	{
		return email;
	}

	public void setEmail(String email)
	{
		this.email = email;
	}

	public String getAanhef()
	{
		return aanhef;
	}

	public void setAanhef(String aanhef)
	{
		this.aanhef = aanhef;
	}

	public String getAchternaam()
	{
		return achternaam;
	}

	public void setAchternaam(String achternaam)
	{
		this.achternaam = achternaam;
	}

	public String getTussenvoegsel()
	{
		return tussenvoegsel;
	}

	public void setTussenvoegsel(String tussenvoegsel)
	{
		this.tussenvoegsel = tussenvoegsel;
	}

	public String getTelefoon()
	{
		return telefoon;
	}

	public void setTelefoon(String telefoon)
	{
		this.telefoon = telefoon;
	}

	public AdresDto getPostadres()
	{
		return postadres;
	}

	public void setPostadres(AdresDto postadres)
	{
		this.postadres = postadres;
	}

	public String getInlogCode()
	{
		return inlogCode;
	}

	public void setInlogCode(String inlogCode)
	{
		this.inlogCode = inlogCode;
	}

	public String getStatus()
	{
		return status;
	}

	public void setStatus(String status)
	{
		this.status = status;
	}

	public String getUsername()
	{
		return username;
	}

	public void setUsername(String username)
	{
		this.username = username;
	}

	public String getWachtwoord()
	{
		return wachtwoord;
	}

	public void setWachtwoord(String wachtwoord)
	{
		this.wachtwoord = wachtwoord;
	}

	public List<LocatieDto> getLocaties()
	{
		return locaties;
	}

	public void setLocaties(List<LocatieDto> locaties)
	{
		this.locaties = locaties;
	}

	public Boolean getActief()
	{
		return actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public String getVoorletters()
	{
		return voorletters;
	}

	public void setVoorletters(String voorletters)
	{
		this.voorletters = voorletters;
	}

	public Date getOvereenkomstGeaccordeerdDatum()
	{
		return overeenkomstGeaccordeerdDatum;
	}

	public void setOvereenkomstGeaccordeerdDatum(Date overeenkomstGeaccordeerdDatum)
	{
		this.overeenkomstGeaccordeerdDatum = overeenkomstGeaccordeerdDatum;
	}

	public Boolean getOvereenkomst()
	{
		return overeenkomst;
	}

	public void setOvereenkomst(Boolean overeenkomst)
	{
		this.overeenkomst = overeenkomst;
	}

	public String getExtraEmails()
	{
		return extraEmails;
	}

	public void setExtraEmails(String extraEmails)
	{
		this.extraEmails = extraEmails;
	}

	public String getAanmeldStatus()
	{
		return aanmeldStatus;
	}

	public void setAanmeldStatus(String aanmeldStatus)
	{
		this.aanmeldStatus = aanmeldStatus;
	}
}
