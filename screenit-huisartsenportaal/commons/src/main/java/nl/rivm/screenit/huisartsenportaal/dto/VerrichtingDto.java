package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-commons
 * %%
 * Copyright (C) 2016 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class VerrichtingDto extends AbstractDtoReferenceObject
{
	private String regio;

	private String monsterId;

	private Date verrichtingsDatum;

	private Date datumUitstrijkje;

	private Date formulierOntvangstDatum;

	private LocatieDto huisartsLocatie;

	private String huisartsLocatieNaam;

	private String clientNaam;

	private List<BetalingDto> betalingen = new ArrayList<>();

	public List<BetalingDto> getBetalingen()
	{
		return betalingen;
	}

	public void setBetalingen(List<BetalingDto> betalingen)
	{
		this.betalingen = betalingen;
	}

	public String getRegio()
	{
		return regio;
	}

	public void setRegio(String regio)
	{
		this.regio = regio;
	}

	public String getMonsterId()
	{
		return monsterId;
	}

	public void setMonsterId(String monsterId)
	{
		this.monsterId = monsterId;
	}

	public Date getVerrichtingsDatum()
	{
		return verrichtingsDatum;
	}

	public void setVerrichtingsDatum(Date verrichtingsDatum)
	{
		this.verrichtingsDatum = verrichtingsDatum;
	}

	public String getClientNaam()
	{
		return clientNaam;
	}

	public void setClientNaam(String clientNaam)
	{
		this.clientNaam = clientNaam;
	}

	public Date getDatumUitstrijkje()
	{
		return datumUitstrijkje;
	}

	public void setDatumUitstrijkje(Date datumUitstrijkje)
	{
		this.datumUitstrijkje = datumUitstrijkje;
	}

	public Date getFormulierOntvangstDatum()
	{
		return formulierOntvangstDatum;
	}

	public void setFormulierOntvangstDatum(Date formulierOntvangstDatum)
	{
		this.formulierOntvangstDatum = formulierOntvangstDatum;
	}

	public String getHuisartsLocatieNaam()
	{
		return huisartsLocatieNaam;
	}

	public void setHuisartsLocatieNaam(String huisartsLocatieNaam)
	{
		this.huisartsLocatieNaam = huisartsLocatieNaam;
	}

	public LocatieDto getHuisartsLocatie()
	{
		return huisartsLocatie;
	}

	public void setHuisartsLocatie(LocatieDto huisartsLocatie)
	{
		this.huisartsLocatie = huisartsLocatie;
	}
}
