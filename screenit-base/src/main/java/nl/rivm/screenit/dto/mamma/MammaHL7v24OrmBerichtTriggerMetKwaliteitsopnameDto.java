package nl.rivm.screenit.dto.mamma;

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

import nl.rivm.screenit.model.mamma.enums.MammaKwaliteitsopnameType;

public class MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto extends MammaAbstractHL7v24OrmBerichtTriggerDto implements Serializable
{
	private MammaKwaliteitsopnameType type;

	private String seCode;

	private String reden;

	private String patientID;

	private String accessionNumber;

	private String onderzoekscode;

	public MammaKwaliteitsopnameType getType()
	{
		return type;
	}

	public void setType(MammaKwaliteitsopnameType type)
	{
		this.type = type;
	}

	public String getSeCode()
	{
		return seCode;
	}

	public void setSeCode(String seCode)
	{
		this.seCode = seCode;
	}

	public String getReden()
	{
		return reden;
	}

	public void setReden(String reden)
	{
		this.reden = reden;
	}

	public String getPatientID()
	{
		return patientID;
	}

	public void setPatientID(String patientID)
	{
		this.patientID = patientID;
	}

	public String getAccessionNumber()
	{
		return accessionNumber;
	}

	public void setAccessionNumber(String accessionNumber)
	{
		this.accessionNumber = accessionNumber;
	}

	public String getOnderzoekscode()
	{
		return onderzoekscode;
	}

	public void setOnderzoekscode(String onderzoekscode)
	{
		this.onderzoekscode = onderzoekscode;
	}
}
