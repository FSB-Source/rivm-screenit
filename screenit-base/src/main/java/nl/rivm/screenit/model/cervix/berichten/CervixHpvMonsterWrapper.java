package nl.rivm.screenit.model.cervix.berichten;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtWaarde;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ca.uhn.hl7v2.model.Varies;
import ca.uhn.hl7v2.model.v251.datatype.CE;
import ca.uhn.hl7v2.model.v251.datatype.DR;
import ca.uhn.hl7v2.model.v251.datatype.EI;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_CONTAINER;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_ORDER;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_RESULT;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_SPECIMEN;
import ca.uhn.hl7v2.model.v251.segment.INV;
import ca.uhn.hl7v2.model.v251.segment.OBX;
import ca.uhn.hl7v2.model.v251.segment.SPM;

public class CervixHpvMonsterWrapper
{
	private static final Logger LOG = LoggerFactory.getLogger(CervixHpvMonsterWrapper.class);

	private OUL_R22_SPECIMEN specimen;

	public CervixHpvMonsterWrapper(OUL_R22_SPECIMEN specimen)
	{
		this.specimen = specimen;
	}

	public String getBarcode()
	{
		SPM spm = specimen.getSPM();
		EI spm2 = spm.getSpm2_SpecimenID().getEip1_PlacerAssignedIdentifier();
		ST spm21 = spm2.getEi1_EntityIdentifier();
		return spm21.getValue();
	}

	public String getControleWaarde()
	{
		OUL_R22_CONTAINER container = specimen.getCONTAINER();
		INV inv = container.getINV();
		CE controle = inv.getInv1_SubstanceIdentifier();
		return controle.getCe1_Identifier().getValue();
	}

	public OUL_R22_SPECIMEN getSpecimen()
	{
		return specimen;
	}

	public String getStringValueAnalyseDatum()
	{
		OUL_R22_ORDER order = specimen.getORDER();
		OUL_R22_RESULT result = order.getRESULT(0);
		OBX obx = result.getOBX();

		Varies obx5_0 = obx.getObx5_ObservationValue(0);
		DR data = (DR) obx5_0.getData();
		return data.getDr1_RangeStartDateTime().getTs1_Time().getValue();
	}

	public Date getAnalyseDatum()
	{
		Date analyseDatum = null;
		try
		{
			String datum = getStringValueAnalyseDatum();
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
			analyseDatum = format.parse(datum);
		}
		catch (ParseException e)
		{
			return null;
		}
		return analyseDatum;
	}

	public String getStringValueAutorisatieDatum()
	{
		OUL_R22_ORDER order = specimen.getORDER();
		OUL_R22_RESULT result = order.getRESULT(0);
		OBX obx = result.getOBX();

		Varies obx5_0 = obx.getObx5_ObservationValue(0);
		DR data = (DR) obx5_0.getData();
		return data.getDr2_RangeEndDateTime().getTs1_Time().getValue();
	}

	public Date getAutorisatieDatum()
	{
		Date autorisatieDatum = null;
		try
		{
			String datum = getStringValueAutorisatieDatum();
			SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
			autorisatieDatum = format.parse(datum);
		}
		catch (ParseException e)
		{
			return null;
		}
		return autorisatieDatum;
	}

	public String getStringValueUitslag()
	{
		OUL_R22_ORDER order = specimen.getORDER();
		OUL_R22_RESULT result = order.getRESULT(1);
		OBX obx = result.getOBX();
		Varies obx5 = obx.getObx5_ObservationValue(0);
		ST data = (ST) obx5.getData();
		return data.getValue();
	}

	public CervixHpvBerichtWaarde getUitslag()
	{
		return CervixHpvBerichtWaarde.fromValue(getStringValueUitslag());
	}

	public boolean isValid()
	{
		LOG.debug("--- Sample ---");
		LOG.debug("Barcode: " + getBarcode());
		LOG.debug("AnalyseDatum: " + getStringValueAnalyseDatum());
		LOG.debug("AutorisatieDatum: " + getStringValueAutorisatieDatum());
		LOG.debug("Uitslag: " + getStringValueUitslag());
		if (getBarcode() != null && getStringValueUitslag() != null && getStringValueAnalyseDatum() != null && getStringValueAutorisatieDatum() != null)
		{
			return true;
		}
		return false;
	}

}
