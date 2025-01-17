package nl.rivm.screenit.model.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v24.datatype.ELD;
import ca.uhn.hl7v2.model.v24.message.ACK;
import ca.uhn.hl7v2.model.v24.segment.ERR;
import ca.uhn.hl7v2.model.v24.segment.MSA;
import ca.uhn.hl7v2.util.StringUtil;

public class ScreenITResponseV24MessageWrapper implements ScreenITResponseHL7v2MessageWrapper
{

	private static final long serialVersionUID = 1L;

	private final AcknowledgmentCode acknowledgmentCode;

	private final String acknowledgmentCodeString;

	private final String melding;

	public ScreenITResponseV24MessageWrapper(Message message)
	{
		this.acknowledgmentCodeString = getFoutCode(message);
		if (acknowledgmentCodeString != null)
		{
			this.acknowledgmentCode = AcknowledgmentCode.valueOf(acknowledgmentCodeString);
		}
		else
		{
			acknowledgmentCode = AcknowledgmentCode.AE;
		}
		String meldingUitBericht = getFoutmelding(message);
		if (StringUtil.isBlank(meldingUitBericht) && acknowledgmentCodeString == null)
		{
			this.melding = "Kon de acknowledgmentcode niet uit het bericht halen.";
		}
		else
		{
			this.melding = getFoutmelding(message);
		}
	}

	@Override
	public AcknowledgmentCode getAcknowledgmentCode()
	{
		return acknowledgmentCode;
	}

	@Override
	public String getMelding()
	{
		return melding;
	}

	private String getFoutmelding(Message response)
	{
		ACK ack = (ACK) response;
		ERR error = ack.getERR();
		ELD eld = error.getErr1_ErrorCodeAndLocation(0);
		return eld.getEld4_CodeIdentifyingError().getText().getValue();
	}

	private String getFoutCode(Message response)
	{
		ACK ack = (ACK) response;
		MSA msa = ack.getMSA();
		return msa.getAcknowledgementCode().getValue();
	}

	@Override
	public String getAcknowledgmentCodeString()
	{
		return acknowledgmentCodeString;
	}

}
