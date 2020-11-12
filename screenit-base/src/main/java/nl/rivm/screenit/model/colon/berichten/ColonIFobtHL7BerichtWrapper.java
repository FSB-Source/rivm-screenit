package nl.rivm.screenit.model.colon.berichten;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.colon.IFOBTResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.Varies;
import ca.uhn.hl7v2.model.v251.datatype.DTM;
import ca.uhn.hl7v2.model.v251.datatype.NM;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_SPECIMEN;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.model.v251.segment.MSH;
import ca.uhn.hl7v2.model.v251.segment.OBX;

public class ColonIFobtHL7BerichtWrapper
{
	private String labId;

	private String messageId;

	private List<IFOBTResult> results = new ArrayList<>();

	private OUL_R22 message;

	private static final Logger LOG = LoggerFactory.getLogger(ColonIFobtHL7BerichtWrapper.class);

	public ColonIFobtHL7BerichtWrapper(OUL_R22 message) throws DataTypeException
	{
		this.message = message;
		MSH header = message.getMSH();
		messageId = header.getMsh10_MessageControlID().getValue();
		labId = header.getMsh4_SendingFacility().getHd1_NamespaceID().getValue();

		for (int i = 0; i < message.getSPECIMENReps(); i++)
		{
			OUL_R22_SPECIMEN specimen = message.getSPECIMEN(i);
			OBX resultOBX = specimen.getORDER().getRESULT().getOBX();

			IFOBTResult ifobtResult = new IFOBTResult();

			ST entityIdentifier = specimen.getCONTAINER().getSAC().getContainerIdentifier().getEntityIdentifier();

			if (entityIdentifier != null)
			{
				ifobtResult.setSid(entityIdentifier.getValue());
			}
			ifobtResult.setLabID(labId);

			Varies observationValue = resultOBX.getObx5_ObservationValue(0);

			if (observationValue.getData() instanceof NM)
			{
				ifobtResult.setResultValue(((NM) observationValue.getData()).getValue());
			}
			else
			{
				throw new DataTypeException("Observation value is niet numeriek. Barcode: " + ifobtResult.getSid());
			}

			ST equipmentInstanceIdentifier = resultOBX.getObx18_EquipmentInstanceIdentifier(0).getEi1_EntityIdentifier();
			if (equipmentInstanceIdentifier != null)
			{
				ifobtResult.setInstrumentID(equipmentInstanceIdentifier.getValue());
			}

			try
			{
				DTM resultDate = resultOBX.getObx19_DateTimeOfTheAnalysis().getTime();
				if (resultDate != null)
				{
					ifobtResult.setDateTimeResult(resultDate.getValueAsDate());
				}
			}
			catch (DataTypeException ex)
			{
				LOG.warn("Fout bij parsen analysedatum FIT. MessageID: " + messageId);
				throw ex;
			}
			ifobtResult.setBestandsNaam(messageId);
			results.add(ifobtResult);
		}

	}

	public String getMessageId()
	{
		return messageId;
	}

	public String getLabId()
	{
		return labId;
	}

	public List<IFOBTResult> getResults()
	{
		return results;
	}

	public OUL_R22 getMessage()
	{
		return message;
	}
}
