package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.List;

public class MammograafStatus
{

	private String aeTitle;

	private LocalDateTime laatsteSuccesDmwlBerichtTimestamp;

	private List<MammograafDicomMessageError> foutenSindsLaatsteSuccesDmwlBericht;

	private LocalDateTime laatsteSuccesMppsBerichtTimestamp;

	private List<MammograafDicomMessageError> foutenSindsLaatsteSuccesMppsBericht;

	private String mammograafDatum;

	public MammograafStatus(String aeTitle)
	{
		this.aeTitle = aeTitle;
		this.foutenSindsLaatsteSuccesDmwlBericht = new ArrayList<>();
		this.foutenSindsLaatsteSuccesMppsBericht = new ArrayList<>();
	}

	public String getAeTitle()
	{
		return aeTitle;
	}

	public void setAeTitle(String aeTitle)
	{
		this.aeTitle = aeTitle;
	}

	public LocalDateTime getLaatsteSuccesDmwlBerichtTimestamp()
	{
		return laatsteSuccesDmwlBerichtTimestamp;
	}

	public void setLaatsteSuccesDmwlBerichtTimestamp(LocalDateTime laatsteSuccesDmwlBerichtTimestamp)
	{
		this.laatsteSuccesDmwlBerichtTimestamp = laatsteSuccesDmwlBerichtTimestamp;
	}

	public List<MammograafDicomMessageError> getFoutenSindsLaatsteSuccesDmwlBericht()
	{
		return foutenSindsLaatsteSuccesDmwlBericht;
	}

	public void setFoutenSindsLaatsteSuccesDmwlBericht(List<MammograafDicomMessageError> foutenSindsLaatsteSuccesDmwlBericht)
	{
		this.foutenSindsLaatsteSuccesDmwlBericht = foutenSindsLaatsteSuccesDmwlBericht;
	}

	public void registreerDmwlFout(String foutMelding)
	{
		this.foutenSindsLaatsteSuccesDmwlBericht.add(new MammograafDicomMessageError(foutMelding));
	}

	public LocalDateTime getLaatsteSuccesMppsBerichtTimestamp()
	{
		return laatsteSuccesMppsBerichtTimestamp;
	}

	public void setLaatsteSuccesMppsBerichtTimestamp(LocalDateTime laatsteSuccesMppsBerichtTimestamp)
	{
		this.laatsteSuccesMppsBerichtTimestamp = laatsteSuccesMppsBerichtTimestamp;
	}

	public List<MammograafDicomMessageError> getFoutenSindsLaatsteSuccesMppsBericht()
	{
		return foutenSindsLaatsteSuccesMppsBericht;
	}

	public void setFoutenSindsLaatsteSuccesMppsBericht(List<MammograafDicomMessageError> foutenSindsLaatsteSuccesMppsBericht)
	{
		this.foutenSindsLaatsteSuccesMppsBericht = foutenSindsLaatsteSuccesMppsBericht;
	}

	public void registreerMppsFout(String foutMelding)
	{
		this.foutenSindsLaatsteSuccesMppsBericht.add(new MammograafDicomMessageError(foutMelding));
	}

	public String getMammograafDatum()
	{
		return mammograafDatum;
	}

	public void setMammograafDatum(String mammograafDatum)
	{
		this.mammograafDatum = mammograafDatum;
	}
}
