package nl.rivm.screenit.model.mamma.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;

import ca.uhn.hl7v2.model.v24.message.ORM_O01;

public class MammaHL7OntvangenBerichtWrapper
{

	private String bsn;

	private Long accessionNumber;

	private String messageId;

	private ORM_O01 message;

	private MammaHL7v24ORMBerichtStatus status;

	public MammaHL7OntvangenBerichtWrapper(ORM_O01 message)
	{
		this.message = message;
		parseMessage(message);
	}

	private void parseMessage(ORM_O01 message)
	{
		bsn = message.getPATIENT().getPID().getPid3_PatientIdentifierList(0).getCx1_ID().getValue();
		accessionNumber = Long.valueOf(message.getORDER().getORDER_DETAIL().getOBR().getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().getValue());
		String statusString = message.getORDER().getORDER_DETAIL().getOBR().getObr25_ResultStatus().getValue();
		status = MammaHL7v24ORMBerichtStatus.getEnumForLabel(statusString);
		messageId = message.getMSH().getMessageControlID().getValue();
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setAccessionNumber(Long accessionNumber)
	{
		this.accessionNumber = accessionNumber;
	}

	public Long getAccessionNumber()
	{
		return accessionNumber;
	}

	public ORM_O01 getMessage()
	{
		return message;
	}

	public void setMessage(ORM_O01 message)
	{
		this.message = message;
	}

	public MammaHL7v24ORMBerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaHL7v24ORMBerichtStatus status)
	{
		this.status = status;
	}

	public String getMessageId()
	{
		return messageId;
	}

	public void setMessageId(String messageId)
	{
		this.messageId = messageId;
	}
}
