package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class KwaliteitsopnameScreenITWerklijstItem extends ScreenITWerklijstItem
{
	private String reden;

	private String voorOfNaKalibratie;

	private String seCode;

	private String accessionNumber;

	private LocalDateTime startMoment;

	private String patientId;

	public String getReden()
	{
		return reden;
	}

	public void setReden(String reden)
	{
		this.reden = reden;
	}

	public String getVoorOfNaKalibratie()
	{
		return voorOfNaKalibratie;
	}

	public void setVoorOfNaKalibratie(String voorOfNaKalibratie)
	{
		this.voorOfNaKalibratie = voorOfNaKalibratie;
	}

	public String getSeCode()
	{
		return seCode;
	}

	public void setSeCode(String seCode)
	{
		this.seCode = seCode;
	}

	public String getAccessionNumber()
	{
		return accessionNumber;
	}

	public void setAccessionNumber(String accessionNumber)
	{
		this.accessionNumber = accessionNumber;
	}

	public LocalDateTime getStartMoment()
	{
		return startMoment;
	}

	public String getPatientId()
	{
		return patientId;
	}

	public void setPatientId(String patientId)
	{
		this.patientId = patientId;
	}

	public void setStartMoment(LocalDateTime startMoment)
	{
		this.startMoment = startMoment;
	}
}
