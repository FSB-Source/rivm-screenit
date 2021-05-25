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

import java.time.LocalDateTime;

import nl.rivm.screenit.mamma.se.dto.onderzoek.MammografieSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.OnderzoekSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.SignalerenSeDto;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;

public class AfspraakSeDto extends SeDto
{
	private LocalDateTime vanaf;

	private ClientSeDto client;

	private AfspraakStatusSe status;

	private Long uitnodigingsNr; 

	private MammaIdentificatiesoort identificatiesoort;

	private String identificatienummer;

	private boolean bezwaarAangevraagd;

	private long aantalOproepen;

	private long aantalOpgekomen;

	private OnderzoekSeDto huidigOnderzoek;

	private MammografieSeDto mammografie;

	private SignalerenSeDto signaleren;

	private Long huisartsId;

	private MammaGeenHuisartsOption geenHuisartsOptie;

	private boolean doorgevoerd;

	private boolean centralAvailable;

	private boolean eerderOnderbrokenInZelfdeRonde;

	private MammaBeoordelingOpschortenReden eerdereOpschortenReden;

	private String eerdereOpschortenRedenTekst;

	private boolean geforceerd;

	public LocalDateTime getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(LocalDateTime vanaf)
	{
		this.vanaf = vanaf;
	}

	public ClientSeDto getClient()
	{
		return client;
	}

	public void setClient(ClientSeDto client)
	{
		this.client = client;
	}

	public AfspraakStatusSe getStatus()
	{
		return status;
	}

	public void setStatus(AfspraakStatusSe status)
	{
		this.status = status;
	}

	public Long getUitnodigingsNr()
	{
		return uitnodigingsNr;
	}

	public void setUitnodigingsNr(Long uitnodigingsNr)
	{
		this.uitnodigingsNr = uitnodigingsNr;
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

	public boolean isBezwaarAangevraagd()
	{
		return bezwaarAangevraagd;
	}

	public void setBezwaarAangevraagd(boolean bezwaarAangevraagd)
	{
		this.bezwaarAangevraagd = bezwaarAangevraagd;
	}

	public long getAantalOproepen()
	{
		return aantalOproepen;
	}

	public void setAantalOproepen(long aantalOproepen)
	{
		this.aantalOproepen = aantalOproepen;
	}

	public long getAantalOpgekomen()
	{
		return aantalOpgekomen;
	}

	public void setAantalOpgekomen(long aantalOpgekomen)
	{
		this.aantalOpgekomen = aantalOpgekomen;
	}

	public OnderzoekSeDto getHuidigOnderzoek()
	{
		return huidigOnderzoek;
	}

	public void setHuidigOnderzoek(OnderzoekSeDto huidigOnderzoek)
	{
		this.huidigOnderzoek = huidigOnderzoek;
	}

	public MammografieSeDto getMammografie()
	{
		return mammografie;
	}

	public void setMammografie(MammografieSeDto mammografie)
	{
		this.mammografie = mammografie;
	}

	public SignalerenSeDto getSignaleren()
	{
		return signaleren;
	}

	public void setSignaleren(SignalerenSeDto signaleren)
	{
		this.signaleren = signaleren;
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

	public boolean isDoorgevoerd()
	{
		return doorgevoerd;
	}

	public void setDoorgevoerd(boolean doorgevoerd)
	{
		this.doorgevoerd = doorgevoerd;
	}

	public boolean isCentralAvailable()
	{
		return centralAvailable;
	}

	public void setCentralAvailable(boolean centralAvailable)
	{
		this.centralAvailable = centralAvailable;
	}

	public boolean isEerderOnderbrokenInZelfdeRonde()
	{
		return eerderOnderbrokenInZelfdeRonde;
	}

	public void setEerderOnderbrokenInZelfdeRonde(boolean eerderOnderbrokenInZelfdeRonde)
	{
		this.eerderOnderbrokenInZelfdeRonde = eerderOnderbrokenInZelfdeRonde;
	}

	public MammaBeoordelingOpschortenReden getEerdereOpschortenReden()
	{
		return eerdereOpschortenReden;
	}

	public void setEerdereOpschortenReden(MammaBeoordelingOpschortenReden eerdereOpschortenReden)
	{
		this.eerdereOpschortenReden = eerdereOpschortenReden;
	}

	public String getEerdereOpschortenRedenTekst()
	{
		return eerdereOpschortenRedenTekst;
	}

	public void setEerdereOpschortenRedenTekst(String eerdereOpschortenRedenTekst)
	{
		this.eerdereOpschortenRedenTekst = eerdereOpschortenRedenTekst;
	}

	public boolean isGeforceerd()
	{
		return geforceerd;
	}

	public void setGeforceerd(boolean geforceerd)
	{
		this.geforceerd = geforceerd;
	}
}
