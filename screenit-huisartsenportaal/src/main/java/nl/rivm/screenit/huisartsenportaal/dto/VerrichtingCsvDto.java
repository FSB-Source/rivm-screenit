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

public class VerrichtingCsvDto
{
	private String huisartsLocatieNaam;

	private String clientNaam;

	private String monsterId;

	private String datumUitstrijkje;

	private String verrichtingsDatum;

	private String formulierOntvangstDatum;

	private String regio;

	public String getHuisartsLocatieNaam()
	{
		return huisartsLocatieNaam;
	}

	public void setHuisartsLocatieNaam(String huisartsLocatieNaam)
	{
		this.huisartsLocatieNaam = huisartsLocatieNaam;
	}

	public String getClientNaam()
	{
		return clientNaam;
	}

	public void setClientNaam(String clientNaam)
	{
		this.clientNaam = clientNaam;
	}

	public String getMonsterId()
	{
		return monsterId;
	}

	public void setMonsterId(String monsterId)
	{
		this.monsterId = monsterId;
	}

	public String getDatumUitstrijkje()
	{
		return datumUitstrijkje;
	}

	public void setDatumUitstrijkje(String datumUitstrijkje)
	{
		this.datumUitstrijkje = datumUitstrijkje;
	}

	public String getVerrichtingsDatum()
	{
		return verrichtingsDatum;
	}

	public void setVerrichtingsDatum(String verrichtingsDatum)
	{
		this.verrichtingsDatum = verrichtingsDatum;
	}

	public String getFormulierOntvangstDatum()
	{
		return formulierOntvangstDatum;
	}

	public void setFormulierOntvangstDatum(String formulierOntvangstDatum)
	{
		this.formulierOntvangstDatum = formulierOntvangstDatum;
	}

	public String getRegio()
	{
		return regio;
	}

	public void setRegio(String regio)
	{
		this.regio = regio;
	}
}
