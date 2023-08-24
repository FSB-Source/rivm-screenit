package nl.rivm.screenit.model.mamma.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7OnderzoeksCode;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;

import org.apache.commons.lang.StringUtils;

import ca.uhn.hl7v2.model.v24.message.ORM_O01;

@Getter
@Setter
public class MammaHL7OntvangenBerichtWrapper
{
	private String bsn;

	private Long accessionNumber;

	private String messageId;

	private ORM_O01 message;

	private MammaHL7v24ORMBerichtStatus status;

	private MammaOnderzoekType onderzoekType;

	public MammaHL7OntvangenBerichtWrapper(ORM_O01 message)
	{
		this.message = message;
		parseMessage(message);
	}

	private void parseMessage(ORM_O01 message)
	{
		bsn = message.getPATIENT().getPID().getPid3_PatientIdentifierList(0).getCx1_ID().getValue();
		accessionNumber = Long.valueOf(message.getORDER().getORDER_DETAIL().getOBR().getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().getValue());
		var statusString = message.getORDER().getORDER_DETAIL().getOBR().getObr25_ResultStatus().getValue();
		status = MammaHL7v24ORMBerichtStatus.getEnumForLabel(statusString);
		messageId = message.getMSH().getMessageControlID().getValue();
		onderzoekType = parseOnderzoekType(message);
	}

	private MammaOnderzoekType parseOnderzoekType(ORM_O01 message)
	{
		var onderzoeksCodeString = message.getORDER().getORDER_DETAIL().getOBR().getObr4_UniversalServiceIdentifier().getCe1_Identifier().getValue();
		if (StringUtils.isBlank(onderzoeksCodeString))
		{
			return null; 
		}

		var onderzoeksCode = MammaHL7OnderzoeksCode.valueOf(onderzoeksCodeString);
		switch (onderzoeksCode)
		{
		case SCREENDBT:
			return MammaOnderzoekType.TOMOSYNTHESE;

		case ZHOND:
		case MAMMO:
			return MammaOnderzoekType.MAMMOGRAFIE;

		default:
			throw new IllegalArgumentException("Ongeldige onderzoekscode: " + onderzoeksCode);
		}
	}
}
