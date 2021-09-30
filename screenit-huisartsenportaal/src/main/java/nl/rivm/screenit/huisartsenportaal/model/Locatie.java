package nl.rivm.screenit.huisartsenportaal.model;

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

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import org.hibernate.envers.Audited;

@Entity
@Audited
public class Locatie extends AbstractReferenceObject
{
	private static final long serialVersionUID = 1L;

	@Column(length = 200, nullable = false)
	private String naam;

	@OneToOne(fetch = FetchType.EAGER)
	private Adres locatieAdres;

	@Column(length = 34, nullable = false)
	private String iban;

	@Column(length = 70, nullable = false)
	private String ibanTenaamstelling;

	@OneToMany(mappedBy = "locatie", fetch = FetchType.LAZY)
	public List<LabformulierAanvraag> aanvragen;

	@Column(nullable = false)
	private String zorgmailklantnummer;

	@ManyToOne(fetch = FetchType.LAZY)
	public Huisarts huisarts;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixLocatieStatus status;

	@Column(length = 255, nullable = true, unique = false)
	private String verificatieCode;

	private Boolean moetVerifierenVoorActivatie = false;

	public Adres getLocatieAdres()
	{
		return locatieAdres;
	}

	public void setLocatieAdres(Adres locatieAdres)
	{
		this.locatieAdres = locatieAdres;
	}

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

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public List<LabformulierAanvraag> getAanvragen()
	{
		return aanvragen;
	}

	public void setAanvragen(List<LabformulierAanvraag> aanvragen)
	{
		this.aanvragen = aanvragen;
	}

	public Huisarts getHuisarts()
	{
		return huisarts;
	}

	public void setHuisarts(Huisarts huisarts)
	{
		this.huisarts = huisarts;
	}

	public String getZorgmailklantnummer()
	{
		return zorgmailklantnummer;
	}

	public void setZorgmailklantnummer(String zorgmailklantnummer)
	{
		this.zorgmailklantnummer = zorgmailklantnummer;
	}

	public CervixLocatieStatus getStatus()
	{
		return status;
	}

	public void setStatus(CervixLocatieStatus status)
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

	public Boolean getMoetVerifierenVoorActivatie()
	{
		return moetVerifierenVoorActivatie;
	}

	public void setMoetVerifierenVoorActivatie(Boolean moetVerifieren)
	{
		this.moetVerifierenVoorActivatie = moetVerifieren;
	}
}
