package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.Date;

public class IFOBTResult implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String labID;

	private String instrumentID;

	private String sid;

	private String resultValue;

	private Date dateTimeResult;

	private String bestandsNaam;

	public String getLabID()
	{
		return labID;
	}

	public void setLabID(String labID)
	{
		this.labID = labID;
	}

	public String getInstrumentID()
	{
		return instrumentID;
	}

	public void setInstrumentID(String instrumentID)
	{
		this.instrumentID = instrumentID;
	}

	public String getResultValue()
	{
		return resultValue;
	}

	public void setResultValue(String resultValue)
	{
		this.resultValue = resultValue;
	}

	public Date getDateTimeResult()
	{
		return dateTimeResult;
	}

	public void setDateTimeResult(Date dateTimeResult)
	{
		this.dateTimeResult = dateTimeResult;
	}

	public String getSid()
	{
		return sid;
	}

	public void setSid(String sid)
	{
		this.sid = sid;
	}

	public void setBestandsNaam(String bestandsNaam)
	{
		this.bestandsNaam = bestandsNaam;
	}

	public String getBestandsNaam()
	{
		return bestandsNaam;
	}
}
