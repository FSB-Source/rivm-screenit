package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.HL7Util;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHL7BaseServiceImpl implements CervixHL7BaseService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHL7BaseServiceImpl.class);

	private DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyyMMddHHmmss");

	private DateTimeFormatter geboortedatumFormatter = DateTimeFormat.forPattern("yyyyMMdd");

	@Autowired
	private HL7BaseSendMessageService sendMessageService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Connection openConnection(BMHKLaboratorium laboratorium, int pogingen, ScreenITHL7MessageContext messageContext) throws HL7Exception
	{
		if (laboratorium.getOrderPort() == null || laboratorium.getOrderHost() == null)
		{
			String melding = "Voor laboratorium " + laboratorium.getNaam() + " zijn geen hl7 host en port gespecificeerd.";
			messageContext.getResponseWrapper().setMelding(melding);
			throw new IllegalStateException(melding);
		}

		sendMessageService.openConnection("BMHK Laboratiorium " + laboratorium.getNaam(), laboratorium.getOrderHost(), laboratorium.getOrderPort(), pogingen,
			messageContext);
		return messageContext.getConnection();
	}

	@Override
	public HL7v24ResponseWrapper sendHL7Message(String hl7Bericht, BMHKLaboratorium bmhkLaboratorium)
	{
		ScreenITHL7MessageContext messageContext = new ScreenITHL7MessageContext(HapiContextType.UTF_8);
		try
		{
			openConnection(bmhkLaboratorium, 1, messageContext);
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
		mshSegment.getDateTimeOfMessage().getTimeOfAnEvent().setValue(dateTimeFormatter.print(currentDateSupplier.getDateTime()));
		return mshSegment;
	}

	@Override
	public PID buildPIDSegment(PID pid, Client client) throws DataTypeException
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
		patientGegevens.getFamilyName().getSurname().setValue(persoon.getNaamEigenPartner());
		patientGegevens.getFamilyName().getOwnSurname().setValue(persoon.getAchternaam());
		patientGegevens.getFamilyName().getOwnSurnamePrefix().setValue(persoon.getTussenvoegsel());
		patientGegevens.getFamilyName().getSurnameFromPartnerSpouse().setValue(persoon.getPartnerAchternaam());
		patientGegevens.getFamilyName().getSurnamePrefixFromPartnerSpouse().setValue(persoon.getPartnerTussenvoegsel());

		pid.getPid7_DateTimeOfBirth().getTimeOfAnEvent().setValue(geboortedatumFormatter.print(new DateTime(persoon.getGeboortedatum())));

		if (Geslacht.MAN == persoon.getGeslacht() || Geslacht.VROUW == persoon.getGeslacht())
		{
			pid.getAdministrativeSex().setValue(HL7Util.getGeslachtFormat(persoon.getGeslacht()));
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
	public OBR buildOBRSegment(OBR obrSegment, CervixMonster monster) throws DataTypeException
	{
		obrSegment.getSetIDOBR().setValue("1");
		obrSegment.getObr4_UniversalServiceIdentifier().getCe1_Identifier().setValue("Cervixcytologie");
		obrSegment.getObr2_PlacerOrderNumber().getEi1_EntityIdentifier().setValue(StringUtils.leftPad(monster.getMonsterId(), 9, "0"));
		obrSegment.getObr2_PlacerOrderNumber().getNamespaceID().setValue("ScreenIT");
		obrSegment.getRequestedDateTime().getTs1_TimeOfAnEvent().setValue(dateTimeFormatter.print(currentDateSupplier.getDateTime()));
		return obrSegment;
	}
}
