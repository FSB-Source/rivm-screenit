package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.time.format.DateTimeFormatter;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.HL7Util;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.Connection;
import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.v24.datatype.XAD;
import ca.uhn.hl7v2.model.v24.datatype.XPN;
import ca.uhn.hl7v2.model.v24.segment.MSH;
import ca.uhn.hl7v2.model.v24.segment.OBR;
import ca.uhn.hl7v2.model.v24.segment.PID;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixHL7BaseServiceImpl implements CervixHL7BaseService
{
	@Autowired
	private HL7BaseSendMessageService sendMessageService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Override
	public Connection openConnection(String laboratoriumNaam, int pogingen, ScreenITHL7MessageContext messageContext) throws HL7Exception
	{
		if (messageContext.getPort() == null || messageContext.getHost() == null)
		{
			String melding = "Voor laboratorium " + laboratoriumNaam + " zijn geen HL7v2 host en/of port gespecificeerd.";
			messageContext.getResponseWrapper().setMelding(melding);
			throw new IllegalStateException(melding);
		}

		sendMessageService.openConnection("'" + laboratoriumNaam + "'", pogingen, messageContext);
		return messageContext.getConnection();
	}

	@Override
	public HL7v24ResponseWrapper sendHL7Message(String hl7Bericht, BMHKLaboratorium laboratorium, OrganisatieParameterKey hostKey, OrganisatieParameterKey portKey)
	{
		ScreenITHL7MessageContext messageContext = new ScreenITHL7MessageContext(HapiContextType.UTF_8);
		try
		{
			messageContext.setHost(organisatieParameterService.getOrganisatieParameter(laboratorium, hostKey));
			messageContext.setPort(organisatieParameterService.getOrganisatieParameter(laboratorium, portKey));
			openConnection(laboratorium.getNaam(), 1, messageContext);
			sendMessageService.sendHL7Message(hl7Bericht, messageContext);
			sendMessageService.discardConnection(messageContext);
		}
		catch (Exception e)
		{
			HL7v24ResponseWrapper responseWrapper = messageContext.getResponseWrapper();
			if (StringUtils.isBlank(responseWrapper.getMelding()))
			{
				responseWrapper.setMelding(e.getMessage());
			}
			LOG.error("HL7-bericht kon niet worden verzonden.", e);
			responseWrapper.setCrashException(e);
		}
		return messageContext.getResponseWrapper();
	}

	@Override
	public MSH buildMessageHeader(MSH mshSegment, String namespaceId) throws DataTypeException
	{
		mshSegment.getSendingFacility().getNamespaceID().setValue("SCREENIT");
		mshSegment.getSendingApplication().getNamespaceID().setValue(namespaceId);
		mshSegment.getDateTimeOfMessage().getTimeOfAnEvent().setValue(getCurrentDateTimeString());
		return mshSegment;
	}

	@Override
	public PID buildPIDSegment(PID pid, Client client) throws DataTypeException
	{
		return buildPIDSegmentWithForcedGender(pid, client, null);
	}

	@Override
	public PID buildPIDSegmentWithForcedGender(PID pid, Client client, Geslacht forcedGender) throws DataTypeException
	{
		GbaPersoon persoon = client.getPersoon();
		BagAdres persoonAdres = persoon.getGbaAdres();

		pid.getSetIDPID().setValue("1");

		pid.getPid2_PatientID();

		pid.getPid3_PatientIdentifierList(0).getCx1_ID().setValue(persoon.getBsn());
		pid.getPid3_PatientIdentifierList(0).getCx5_IdentifierTypeCode().setValue("NNNLD");

		XPN patientGegevens = pid.getPid5_PatientName(0);
		patientGegevens.getGivenName().setValue(persoon.getVoornaam());
		patientGegevens.getNameTypeCode().setValue("L");
		patientGegevens.getFamilyName().getSurname().setValue(NaamUtil.getAanspreekTussenvoegselEnAchternaam(client));
		patientGegevens.getFamilyName().getOwnSurname().setValue(persoon.getAchternaam());
		patientGegevens.getFamilyName().getOwnSurnamePrefix().setValue(persoon.getTussenvoegsel());
		patientGegevens.getFamilyName().getSurnameFromPartnerSpouse().setValue(persoon.getPartnerAchternaam());
		patientGegevens.getFamilyName().getSurnamePrefixFromPartnerSpouse().setValue(persoon.getPartnerTussenvoegsel());

		pid.getPid7_DateTimeOfBirth().getTimeOfAnEvent().setValue(DateUtil.formatForPattern(Constants.DATE_FORMAT_YYYYMMDD, persoon.getGeboortedatum()));

		if (persoon.getGeslacht() != null)
		{
			if (forcedGender != null)
			{
				pid.getAdministrativeSex().setValue(HL7Util.getGeslachtFormat(forcedGender));
			}
			else
			{
				pid.getAdministrativeSex().setValue(HL7Util.getGeslachtFormat(persoon.getGeslacht()));
			}
		}

		XAD omlAdres1 = pid.getPatientAddress(0);
		omlAdres1.getCity().setValue(persoonAdres.getPlaats());
		omlAdres1.getStreetAddress().getSad2_StreetName().setValue(persoonAdres.getStraat());
		if (persoonAdres.getHuisnummer() != null)
		{
			omlAdres1.getStreetAddress().getSad3_DwellingNumber().setValue(persoonAdres.getHuisnummer().toString());
		}
		omlAdres1.getZipOrPostalCode().setValue(persoonAdres.getPostcode());
		omlAdres1.getXad11_AddressRepresentationCode().setValue("A");
		omlAdres1.getCountry().setValue("NLD");
		omlAdres1.getAddressType().setValue("L");

		pid.getPid12_CountyCode().setValue("NLD");

		pid.getPid23_BirthPlace().setValue(persoon.getGeboorteplaats());
		return pid;
	}

	@Override
	public OBR buildOBRSegment(OBR obrSegment, CervixMonster monster, boolean postfix) throws DataTypeException
	{
		obrSegment.getSetIDOBR().setValue("1");
		obrSegment.getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(CervixMonsterUtil.getMonsterEntityIdentifier(monster, postfix));
		obrSegment.getObr2_PlacerOrderNumber().getNamespaceID().setValue("ScreenIT");
		return obrSegment;
	}

	@Override
	public String getCurrentDateTimeString()
	{
		return currentDateSupplier.getLocalDateTime().format(DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDDHHMMSS));
	}
}
