package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium.facturatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;

public class CervixLaboratoriumVerrichtingTotalenViewObject implements Serializable
{
	private String hpvAnalyseUitstrijkjeTariefTotaal;

	private String hpvAnalyseZasTariefTotaal;

	private String cytologieNaHpvUitstrijkjeTariefTotaal;

	private String cytologieNaHpvZasTariefTotaal;

	private String cytologieVervolguitstrijkjeTariefTotaal;

	private String totaalBedrag = "€ 0,00";

	public String getHpvAnalyseUitstrijkjeTariefTotaal()
	{
		return hpvAnalyseUitstrijkjeTariefTotaal;
	}

	public void setHpvAnalyseUitstrijkjeTariefTotaal(String hpvAnalyseUitstrijkjeTariefTotaal)
	{
		this.hpvAnalyseUitstrijkjeTariefTotaal = hpvAnalyseUitstrijkjeTariefTotaal;
	}

	public String getHpvAnalyseZasTariefTotaal()
	{
		return hpvAnalyseZasTariefTotaal;
	}

	public void setHpvAnalyseZasTariefTotaal(String hpvAnalyseZasTariefTotaal)
	{
		this.hpvAnalyseZasTariefTotaal = hpvAnalyseZasTariefTotaal;
	}

	public String getCytologieNaHpvUitstrijkjeTariefTotaal()
	{
		return cytologieNaHpvUitstrijkjeTariefTotaal;
	}

	public void setCytologieNaHpvUitstrijkjeTariefTotaal(String cytologieNaHpvUitstrijkjeTariefTotaal)
	{
		this.cytologieNaHpvUitstrijkjeTariefTotaal = cytologieNaHpvUitstrijkjeTariefTotaal;
	}

	public String getCytologieNaHpvZasTariefTotaal()
	{
		return cytologieNaHpvZasTariefTotaal;
	}

	public void setCytologieNaHpvZasTariefTotaal(String cytologieNaHpvZasTariefTotaal)
	{
		this.cytologieNaHpvZasTariefTotaal = cytologieNaHpvZasTariefTotaal;
	}

	public String getCytologieVervolguitstrijkjeTariefTotaal()
	{
		return cytologieVervolguitstrijkjeTariefTotaal;
	}

	public void setCytologieVervolguitstrijkjeTariefTotaal(String cytologieVervolguitstrijkjeTariefTotaal)
	{
		this.cytologieVervolguitstrijkjeTariefTotaal = cytologieVervolguitstrijkjeTariefTotaal;
	}

	public String getTotaalBedrag()
	{
		return totaalBedrag;
	}

	public void setTotaalBedrag(String totaalBedrag)
	{
		this.totaalBedrag = totaalBedrag;
	}
}
