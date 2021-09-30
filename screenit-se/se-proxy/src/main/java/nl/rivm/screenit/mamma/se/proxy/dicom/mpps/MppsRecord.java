package nl.rivm.screenit.mamma.se.proxy.dicom.mpps;

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

public class MppsRecord
{
	private final String sopInstanceUid;

	private String patientName;

	private String patientID;

	private String accessionNumber;

	private String status;

	private String discontinuedReason;

	private boolean beeldenLinks = false;

	private boolean beeldenRechts = false;

	void setBeeldenLinks(boolean beeldenLinks)
	{
		this.beeldenLinks = beeldenLinks;
	}

	void setBeeldenRechts(boolean beeldenRechts)
	{
		this.beeldenRechts = beeldenRechts;
	}

	public boolean hasBeeldenLinks()
	{
		return beeldenLinks;
	}

	public boolean hasBeeldenRechts()
	{
		return beeldenRechts;
	}

	MppsRecord(String sopInstanceUid)
	{
		this.sopInstanceUid = sopInstanceUid;
	}

	void setPatientName(String patientName)
	{
		this.patientName = patientName;
	}

	void setPatientID(String patientID)
	{
		this.patientID = patientID;
	}

	void setAccessionNumber(String accessionNumber)
	{
		this.accessionNumber = accessionNumber;
	}

	public String getAccessionNumber()
	{
		return accessionNumber;
	}

	void setStatus(String status)
	{
		this.status = status;
	}

	void setDiscontinuedReason(String discontinuedReason)
	{
		this.discontinuedReason = discontinuedReason;
	}

	@Override
	public String toString()
	{
		String statusTekst = discontinuedReason == null ? status : String.format("%s (%s)", status, discontinuedReason);
		return String.format("SopInstanceUid: %s Patient: %s (%s), Studie-id: %s, Status: %s, %s",
			sopInstanceUid, patientName, patientID, accessionNumber, statusTekst, beeldenZijdeString());
	}

	private String beeldenZijdeString()
	{
		if (beeldenRechts && beeldenLinks)
		{
			return "Beelden: rechts en links";
		}
		else if (beeldenRechts)
		{
			return "Beelden: rechts";
		}
		else if (beeldenLinks)
		{
			return "Beelden: links";
		}
		else
		{
			return "Beelden: geen zijde";
		}
	}
}
