package nl.rivm.screenit.batch.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.IOException;

import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.llp.LLPException;
import ca.uhn.hl7v2.model.Message;

public interface HL7BaseSendMessageService
{
	void openConnection(String connectieNaam, String host, String port, int pogingen, ScreenITHL7MessageContext messageContext) throws HL7Exception;

	HL7v24ResponseWrapper sendHL7Message(String hl7Bericht, ScreenITHL7MessageContext messageContext) throws HL7Exception, LLPException, IOException;

	HL7v24ResponseWrapper sendHL7Message(Message hl7Bericht, ScreenITHL7MessageContext messageContext) throws HL7Exception, LLPException, IOException;

	void discardConnection(ScreenITHL7MessageContext messageContext);

}
