package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.batch.service.CervixHpvOruBerichtService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.Varies;
import ca.uhn.hl7v2.model.v24.datatype.FT;
import ca.uhn.hl7v2.model.v24.group.ORU_R01_ORDER_OBSERVATION;
import ca.uhn.hl7v2.model.v24.group.ORU_R01_PATIENT_RESULT;
import ca.uhn.hl7v2.model.v24.message.ORU_R01;
import ca.uhn.hl7v2.model.v24.segment.OBR;
import ca.uhn.hl7v2.model.v24.segment.OBX;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixHpvOruBerichtServiceImpl implements CervixHpvOruBerichtService
{
	@Autowired
	private CervixHL7BaseService hl7BaseService;

	private static final String OBX1_HPV_POSITIEF = "3";

	private static final String OBX1_HPV_NEGATIEF = "4";

	private static final String OBX2_GEEN_ZAS = "0";

	private static final String OBX2_WEL_ZAS = "1";

	@Override
	public ORU_R01 maakHpvOruBericht(CervixMonster monster)
	{
		ORU_R01 oruBericht = new ORU_R01();

		try
		{

			oruBericht.initQuickstart("ORU", "R01", "P");

			hl7BaseService.buildMessageHeader(oruBericht.getMSH(), "SCREENIT_ORU");

			ORU_R01_PATIENT_RESULT oruPatientResult = oruBericht.getPATIENT_RESULT();

			Client client = monster.getOntvangstScreeningRonde().getDossier().getClient();
			hl7BaseService.buildPIDSegmentWithForcedGender(oruPatientResult.getPATIENT().getPID(), client, Geslacht.VROUW);

			ORU_R01_ORDER_OBSERVATION orderObservation = oruPatientResult.getORDER_OBSERVATION();
			OBR obr = hl7BaseService.buildOBRSegment(orderObservation.getOBR(), monster, false);
			obr.getRequestedDateTime().getTs1_TimeOfAnEvent().setValue(hl7BaseService.getCurrentDateTimeString());
			obr.getObr4_UniversalServiceIdentifier().getCe1_Identifier().setValue("Cervixcytologie");

			OBX obx1 = orderObservation.getOBSERVATION(0).getOBX();
			obx1.getSetIDOBX().setValue("1");
			obx1.getValueType().setValue("FT");

			obx1.getObservationIdentifier().getIdentifier().setValue("UITSL1");
			obx1.getObservationIdentifier().getText().setValue("CR_HPV");
			setObx1_HpvUitslag_ObservationValue(oruBericht, monster, obx1);

			obx1.getObservationResultStatus().setValue("F");

			OBX obx2 = orderObservation.getOBSERVATION(1).getOBX();
			obx2.getSetIDOBX().setValue("2");
			obx2.getValueType().setValue("FT");
			obx2.getObservationIdentifier().getIdentifier().setValue("ZAS");
			setObx2_MonsterType_ObservationValue(oruBericht, monster, obx2);
			obx2.getObservationResultStatus().setValue("F");

		}
		catch (HL7Exception | IOException e)
		{
			LOG.error("Er is een fout opgetreden bij het maken van een order!", e);
		}
		return oruBericht;
	}

	private void setObx1_HpvUitslag_ObservationValue(ORU_R01 oruBericht, CervixMonster monster, OBX obx1) throws DataTypeException
	{
		FT hpvUitslagWaarde = new FT(oruBericht);
		if (CervixHpvBeoordelingWaarde.POSITIEF.equals(monster.getLaatsteHpvBeoordeling().getHpvUitslag()))
		{
			hpvUitslagWaarde.setValue(OBX1_HPV_POSITIEF);
		}
		else if (CervixHpvBeoordelingWaarde.NEGATIEF.equals(monster.getLaatsteHpvBeoordeling().getHpvUitslag()))
		{
			hpvUitslagWaarde.setValue(OBX1_HPV_NEGATIEF);
		}
		else
		{
			throw new IllegalArgumentException("Hpv uitslag mag alleen positief of negatief zijn bij opstellen ORU bericht!");
		}
		Varies value = obx1.getObservationValue(0);
		value.setData(hpvUitslagWaarde);
	}

	private void setObx2_MonsterType_ObservationValue(ORU_R01 oruBericht, CervixMonster monster, OBX obx2) throws DataTypeException
	{
		FT zasUitslag = new FT(oruBericht);
		if (CervixMonsterType.UITSTRIJKJE.equals(monster.getUitnodiging().getMonsterType()))
		{
			zasUitslag.setValue(OBX2_GEEN_ZAS);
		}
		else
		{
			zasUitslag.setValue(OBX2_WEL_ZAS);
		}
		Varies value = obx2.getObservationValue(0);
		value.setData(zasUitslag);
	}
}
