package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-commons
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

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
public class LocatieDto extends AbstractDtoReferenceObject
{
	@NotNull(message = "IBAN is verplicht.")
	@Size(max = 34)
	private String iban;

	@NotNull(message = "Tenaamstelling is verplicht.")
	@Size(max = 70, message = "Tenaamstelling is te lang.")
	private String ibanTenaamstelling;

	@NotNull(message = "Locatienaam is verplicht.")
	@Size(max = 70, message = "Locatienaam is te lang.")
	private String naam;

	@Valid
	@NotNull(message = "Locatieadres is verplicht.")
	private AdresDto locatieAdres = new AdresDto();

	@NotNull
	private String status;

	private Boolean herzendVerificatieMail = null;

	@NotNull(message = "Zorgmail klantnummer is verplicht.")
	@Pattern(regexp = "[0-9]{9,9}", message = "Zorgmail klantnummer voldoet niet aan het verplichte formaat")
	private String zorgmailklantnummer;

	private Long huisartsId;

	private String verificatieCode;

	private Boolean moetVerifierenVoorActivatie = false;

	public String getIban()
	{
		return iban;
	}

	public void setIban(String iban)
	{
		this.iban = iban;
	}

	public String getIbanTenaamstelling()
	{
		return ibanTenaamstelling;
	}

	public void setIbanTenaamstelling(String ibanTenaamstelling)
	{
		this.ibanTenaamstelling = ibanTenaamstelling;
	}

	public AdresDto getLocatieAdres()
	{
		return locatieAdres;
	}

	public void setLocatieAdres(AdresDto locatieAdres)
	{
		this.locatieAdres = locatieAdres;
	}

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public String getZorgmailklantnummer()
	{
		return zorgmailklantnummer;
	}

	public void setZorgmailklantnummer(String zorgmailklantnummer)
	{
		this.zorgmailklantnummer = zorgmailklantnummer;
	}

	public Long getHuisartsId()
	{
		return huisartsId;
	}

	public void setHuisartsId(Long huisartsId)
	{
		this.huisartsId = huisartsId;
	}

	public String getStatus()
	{
		return status;
	}

	public void setStatus(String status)
	{
		this.status = status;
	}

	public String getVerificatieCode()
	{
		return verificatieCode;
	}

	public void setVerificatieCode(String verificatieCode)
	{
		this.verificatieCode = verificatieCode;
	}

	public Boolean getHerzendVerificatieMail()
	{
		return herzendVerificatieMail;
	}

	public void setHerzendVerificatieMail(Boolean herzendVerificatieMail)
	{
		this.herzendVerificatieMail = herzendVerificatieMail;
	}

	public Boolean getMoetVerifierenVoorActivatie()
	{
		return moetVerifierenVoorActivatie;
	}

	public void setMoetVerifierenVoorActivatie(Boolean moetVerifierenVoorActivatie)
	{
		this.moetVerifierenVoorActivatie = moetVerifierenVoorActivatie;
	}
}
