
package nl.rivm.screenit.model.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

public class LoggingZoekCriteria implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String gebruikersnaam;

	private Date vanaf;

	private Date tot;

	private String melding;

	private List<LogGebeurtenis> gebeurtenis;

	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	private Long screeningsEenheidId;

	private List<Level> level;

	private String bsnClient;

	private Long regio;

	public String getGebruikersnaam()
	{
		return gebruikersnaam;
	}

	public void setGebruikersnaam(String gebruikersnaam)
	{
		this.gebruikersnaam = gebruikersnaam;
	}

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTot()
	{
		return tot;
	}

	public void setTot(Date tot)
	{
		this.tot = tot;
	}

	public List<LogGebeurtenis> getGebeurtenis()
	{
		return gebeurtenis;
	}

	public void setGebeurtenis(List<LogGebeurtenis> gebeurtenis)
	{
		this.gebeurtenis = gebeurtenis;
	}

	public String getBsnClient()
	{
		return bsnClient;
	}

	public void setBsnClient(String bsnClient)
	{
		this.bsnClient = bsnClient;
	}

	public List<Level> getLevel()
	{
		return level;
	}

	public void setLevel(List<Level> level)
	{
		this.level = level;
	}

	public String getMelding()
	{
		return melding;
	}

	public void setMelding(String melding)
	{
		this.melding = melding;
	}

	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

	public void setBevolkingsonderzoeken(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
	}

	public Long getRegio()
	{
		return regio;
	}

	public void setRegio(Long regio)
	{
		this.regio = regio;
	}

	public Long getScreeningsEenheidId()
	{
		return screeningsEenheidId;
	}

	public void setScreeningsEenheidId(Long screeningsEenheidId)
	{
		this.screeningsEenheidId = screeningsEenheidId;
	}
}
