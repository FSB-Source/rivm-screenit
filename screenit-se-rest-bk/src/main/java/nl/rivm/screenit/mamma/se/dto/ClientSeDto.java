package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.onderzoek.VorigOnderzoekDto;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;

public class ClientSeDto extends SeDto
{
	private String voorletters;

	private String geboorteTussenvoegsel;

	private String geboorteAchternaam;

	private String aanspreekTussenvoegselEnAchternaam;

	private String bsn;

	private LocalDate geboortedatum;

	private String geslacht;

	private AdresSeDto adres;

	private String emailadres;

	private AdresSeDto tijdelijkGbaAdres;

	private TijdelijkAdresSeDto tijdelijkAdres;

	private String telefoonnummer1;

	private String telefoonnummer2;

	private MammaDoelgroep doelgroep;

	private boolean inTehuis;

	private String dubbeleTijdReden;

	private List<VorigOnderzoekDto> vorigeOnderzoeken;

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public LocalDate getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(LocalDate newValue)
	{
		this.geboortedatum = newValue;
	}

	public String getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(String geslacht)
	{
		this.geslacht = geslacht;
	}

	public String getVoorletters()
	{
		return voorletters;
	}

	public void setVoorletters(String voorletters)
	{
		this.voorletters = voorletters;
	}

	public String getGeboorteTussenvoegsel()
	{
		return geboorteTussenvoegsel;
	}

	public void setGeboorteTussenvoegsel(String geboorteTussenvoegsel)
	{
		this.geboorteTussenvoegsel = geboorteTussenvoegsel;
	}

	public String getGeboorteAchternaam()
	{
		return geboorteAchternaam;
	}

	public void setGeboorteAchternaam(String geboorteAchternaam)
	{
		this.geboorteAchternaam = geboorteAchternaam;
	}

	public String getAanspreekTussenvoegselEnAchternaam()
	{
		return aanspreekTussenvoegselEnAchternaam;
	}

	public void setAanspreekTussenvoegselEnAchternaam(String aanspreekTussenvoegselEnAchternaam)
	{
		this.aanspreekTussenvoegselEnAchternaam = aanspreekTussenvoegselEnAchternaam;
	}

	public AdresSeDto getAdres()
	{
		return adres;
	}

	public void setAdres(AdresSeDto adres)
	{
		this.adres = adres;
	}

	public String getTelefoonnummer1()
	{
		return telefoonnummer1;
	}

	public void setTelefoonnummer1(String telefoonnummer1)
	{
		this.telefoonnummer1 = telefoonnummer1;
	}

	public String getTelefoonnummer2()
	{
		return telefoonnummer2;
	}

	public void setTelefoonnummer2(String telefoonnummer2)
	{
		this.telefoonnummer2 = telefoonnummer2;
	}

	public AdresSeDto getTijdelijkGbaAdres()
	{
		return tijdelijkGbaAdres;
	}

	public void setTijdelijkGbaAdres(AdresSeDto tijdelijkGbaAdres)
	{
		this.tijdelijkGbaAdres = tijdelijkGbaAdres;
	}

	public TijdelijkAdresSeDto getTijdelijkAdres()
	{
		return tijdelijkAdres;
	}

	public void setTijdelijkAdres(TijdelijkAdresSeDto tijdelijkAdres)
	{
		this.tijdelijkAdres = tijdelijkAdres;
	}

	public String getEmailadres()
	{
		return emailadres;
	}

	public void setEmailadres(String emailadres)
	{
		this.emailadres = emailadres;
	}

	public MammaDoelgroep getDoelgroep()
	{
		return doelgroep;
	}

	public void setDoelgroep(MammaDoelgroep doelgroep)
	{
		this.doelgroep = doelgroep;
	}

	public boolean isInTehuis()
	{
		return inTehuis;
	}

	public void setInTehuis(boolean inTehuis)
	{
		this.inTehuis = inTehuis;
	}

	public String getDubbeleTijdReden()
	{
		return dubbeleTijdReden;
	}

	public void setDubbeleTijdReden(String dubbeleTijdReden)
	{
		this.dubbeleTijdReden = dubbeleTijdReden;
	}

	public List<VorigOnderzoekDto> getVorigeOnderzoeken()
	{
		return vorigeOnderzoeken;
	}

	public void setVorigeOnderzoeken(List<VorigOnderzoekDto> vorigeOnderzoeken)
	{
		this.vorigeOnderzoeken = vorigeOnderzoeken;
	}
}
