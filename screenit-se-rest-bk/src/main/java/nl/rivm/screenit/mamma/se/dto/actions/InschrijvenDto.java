package nl.rivm.screenit.mamma.se.dto.actions;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.TijdelijkAdresSeDto;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;

public class InschrijvenDto extends AbstractActionDto
{
	private long afspraakId;

	private MammaIdentificatiesoort identificatiesoort;

	private String identificatienummer;

	private String emailadres;

	private String telefoonnummer1;

	private String telefoonnummer2;

	private boolean bezwaarAangevraagd;

	private Long huisartsId;

	private TijdelijkAdresSeDto tijdelijkAdres;

	private MammaGeenHuisartsOption geenHuisartsOptie;

	public InschrijvenDto()
	{
		super(SEActieType.INSCHRIJVEN);
	}

	public TijdelijkAdresSeDto getTijdelijkAdres()
	{
		return tijdelijkAdres;
	}

	public void setTijdelijkAdres(TijdelijkAdresSeDto tijdelijkAdresDto)
	{
		this.tijdelijkAdres = tijdelijkAdresDto;
	}

	public long getAfspraakId()
	{
		return afspraakId;
	}

	public void setAfspraakId(long newValue)
	{
		afspraakId = newValue;
	}

	public MammaIdentificatiesoort getIdentificatiesoort()
	{
		return identificatiesoort;
	}

	public void setIdentificatiesoort(MammaIdentificatiesoort identificatiesoort)
	{
		this.identificatiesoort = identificatiesoort;
	}

	public String getIdentificatienummer()
	{
		return identificatienummer;
	}

	public void setIdentificatienummer(String identificatienummer)
	{
		this.identificatienummer = identificatienummer;
	}

	public String getEmailadres()
	{
		return emailadres;
	}

	public void setEmailadres(String emailadres)
	{
		this.emailadres = emailadres;
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

	public boolean getBezwaarAangevraagd()
	{
		return bezwaarAangevraagd;
	}

	public void setBezwaarAangevraagd(boolean bezwaarAangevraagd)
	{
		this.bezwaarAangevraagd = bezwaarAangevraagd;
	}

	public Long getHuisartsId()
	{
		return huisartsId;
	}

	public void setHuisartsId(Long huisartsId)
	{
		this.huisartsId = huisartsId;
	}

	public MammaGeenHuisartsOption getGeenHuisartsOptie()
	{
		return geenHuisartsOptie;
	}

	public void setGeenHuisartsOptie(MammaGeenHuisartsOption geenHuisartsOptie)
	{
		this.geenHuisartsOptie = geenHuisartsOptie;
	}
}
