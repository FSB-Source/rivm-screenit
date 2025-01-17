package nl.rivm.screenit.mamma.se.dto.onderzoek;

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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.mamma.se.dto.LezingSeDto;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.util.KeyValue;

public class VorigOnderzoekDto
{
	private LocalDateTime eersteBeeindigdeAfspraakOp;

	private LocalDateTime onderzoekDatum;

	private long uitnodigingsNr;

	private String uitvoerendMbber;

	private String extraMedewerker;

	private Boolean uitslagGunstig;

	private Boolean onbeoordeelbaar;

	private OnderzoekSeDto onderzoek;

	private List<OnderbrokenOnderzoekOption> meerdereOnderzoekenInRondeOnderbrokenRedenen;

	private List<MammaBeoordelingOpschortenReden> meerdereOnderzoekenInRondeOpschortRedenen;

	private List<LezingSeDto> lezingen;

	private LezingSeDto verslagLezing;

	private AnnotatieAfbeeldingSeDto visueleInspectieAfbeelding;

	private SignalerenSeDto signaleren;

	private List<KeyValue> teksten;

	private String nevenbevindingen;

	private List<String> nevenbevindingenOpmerkingen;

	private Boolean beeldenBeschikbaar;

	public LocalDateTime getEersteBeeindigdeAfspraakOp()
	{
		return eersteBeeindigdeAfspraakOp;
	}

	public void setEersteBeeindigdeAfspraakOp(LocalDateTime eersteBeeindigdeAfspraakOp)
	{
		this.eersteBeeindigdeAfspraakOp = eersteBeeindigdeAfspraakOp;
	}

	public long getUitnodigingsNr()
	{
		return uitnodigingsNr;
	}

	public void setUitnodigingsNr(long uitnodigingsNr)
	{
		this.uitnodigingsNr = uitnodigingsNr;
	}

	public String getUitvoerendMbber()
	{
		return uitvoerendMbber;
	}

	public void setUitvoerendMbber(String uitvoerendMbber)
	{
		this.uitvoerendMbber = uitvoerendMbber;
	}

	public String getExtraMedewerker()
	{
		return extraMedewerker;
	}

	public void setExtraMedewerker(String extraMedewerker)
	{
		this.extraMedewerker = extraMedewerker;
	}

	public Boolean getUitslagGunstig()
	{
		return uitslagGunstig;
	}

	public void setUitslagGunstig(Boolean uitslagGunstig)
	{
		this.uitslagGunstig = uitslagGunstig;
	}

	public Boolean isOnbeoordeelbaar()
	{
		return onbeoordeelbaar;
	}

	public void setOnbeoordeelbaar(Boolean onbeoordeelbaar)
	{
		this.onbeoordeelbaar = onbeoordeelbaar;
	}

	public OnderzoekSeDto getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(OnderzoekSeDto onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public List<OnderbrokenOnderzoekOption> getMeerdereOnderzoekenInRondeOnderbrokenRedenen()
	{
		return meerdereOnderzoekenInRondeOnderbrokenRedenen;
	}

	public void setMeerdereOnderzoekenInRondeOnderbrokenRedenen(List<OnderbrokenOnderzoekOption> meerdereOnderzoekenInRondeOnderbrokenRedenen)
	{
		this.meerdereOnderzoekenInRondeOnderbrokenRedenen = meerdereOnderzoekenInRondeOnderbrokenRedenen;
	}

	public List<MammaBeoordelingOpschortenReden> getMeerdereOnderzoekenInRondeOpschortRedenen()
	{
		return meerdereOnderzoekenInRondeOpschortRedenen;
	}

	public void setMeerdereOnderzoekenInRondeOpschortRedenen(List<MammaBeoordelingOpschortenReden> meerdereOnderzoekenInRondeOpschortRedenen)
	{
		this.meerdereOnderzoekenInRondeOpschortRedenen = meerdereOnderzoekenInRondeOpschortRedenen;
	}

	public List<LezingSeDto> getLezingen()
	{
		return lezingen;
	}

	public void setLezingen(List<LezingSeDto> lezingen)
	{
		this.lezingen = lezingen;
	}

	public LezingSeDto getVerslagLezing()
	{
		return verslagLezing;
	}

	public void setVerslagLezing(LezingSeDto verslagLezing)
	{
		this.verslagLezing = verslagLezing;
	}

	public AnnotatieAfbeeldingSeDto getVisueleInspectieAfbeelding()
	{
		return visueleInspectieAfbeelding;
	}

	public void setVisueleInspectieAfbeelding(AnnotatieAfbeeldingSeDto visueleInspectieAfbeelding)
	{
		this.visueleInspectieAfbeelding = visueleInspectieAfbeelding;
	}

	public SignalerenSeDto getSignaleren()
	{
		return signaleren;
	}

	public void setSignaleren(SignalerenSeDto signaleren)
	{
		this.signaleren = signaleren;
	}

	public List<KeyValue> getTeksten()
	{
		return teksten;
	}

	public void setTeksten(List<KeyValue> teksten)
	{
		this.teksten = teksten;
	}

	public void setNevenbevindingen(String nevenbevindingen)
	{
		this.nevenbevindingen = nevenbevindingen;
	}

	public String getNevenbevindingen()
	{
		return nevenbevindingen;
	}

	public List<String> getNevenbevindingenOpmerkingen()
	{
		return nevenbevindingenOpmerkingen;
	}

	public void setNevenbevindingenOpmerkingen(List<String> nevenbevindingenOpmerkingen)
	{
		this.nevenbevindingenOpmerkingen = nevenbevindingenOpmerkingen;
	}

	public Boolean getBeeldenBeschikbaar()
	{
		return beeldenBeschikbaar;
	}

	public void setBeeldenBeschikbaar(Boolean beeldenBeschikbaar)
	{
		this.beeldenBeschikbaar = beeldenBeschikbaar;
	}

	public LocalDateTime getOnderzoekDatum()
	{
		return onderzoekDatum;
	}

	public void setOnderzoekDatum(LocalDateTime onderzoekDatum)
	{
		this.onderzoekDatum = onderzoekDatum;
	}
}
