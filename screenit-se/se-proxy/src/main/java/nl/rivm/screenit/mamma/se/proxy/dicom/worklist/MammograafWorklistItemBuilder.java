package nl.rivm.screenit.mamma.se.proxy.dicom.worklist;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;

import nl.rivm.screenit.mamma.se.proxy.model.ClientScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.model.KwaliteitsopnameScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.model.ScreenITWerklijstItem;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.DatePrecision;
import org.dcm4che3.data.PersonName;
import org.dcm4che3.data.Sequence;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.VR;

import static org.dcm4che3.data.Tag.PatientName;

class MammograafWorklistItemBuilder
{
	private static final String SCREENIT_BK_MAMMOGRAAF_UID = "0.4.0.127.0.14.1.3.3.1.";

	private final ScreenITWerklijstItem screenITWerklijstItem;

	private static DateTimeFormatter UUID_KWALITEITSOPNAME_LOCAL_DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

	MammograafWorklistItemBuilder(ScreenITWerklijstItem screenITWerklijstItem)
	{
		this.screenITWerklijstItem = screenITWerklijstItem;
	}

	Attributes getMammograafWorklistItem()
	{
		if (screenITWerklijstItem instanceof ClientScreenITWerklijstItem)
		{
			return createClientMammograafWorklistItem((ClientScreenITWerklijstItem) screenITWerklijstItem);
		}
		else if (screenITWerklijstItem instanceof KwaliteitsopnameScreenITWerklijstItem)
		{
			return createKwaliteitsopnameMammograafWorklistItem((KwaliteitsopnameScreenITWerklijstItem) screenITWerklijstItem);
		}
		else
		{
			return null;
		}
	}

	private Attributes createKwaliteitsopnameMammograafWorklistItem(KwaliteitsopnameScreenITWerklijstItem seItem)
	{
		Attributes worklistItem = createWorklistItem();

		var risCode = seItem.getOnderzoekscode();
		addScheduledProcedureStep(worklistItem, seItem, DateUtil.toUtilDate(seItem.getStartMoment()), "Kwaliteitsopname", risCode);

		worklistItem.setString(Tag.AccessionNumber, VR.SH, seItem.getAccessionNumber());
		worklistItem.setString(Tag.ReferringPhysicianName, VR.PN, seItem.getMedewerkercode());

		PersonName name = new PersonName();
		name.set(PersonName.Component.GivenName, "DUMMY");
		name.set(PersonName.Component.MiddleName, "");
		name.set(PersonName.Component.FamilyName, seItem.getSeCode());
		worklistItem.setString(PatientName, VR.PN, name.toString());
		worklistItem.setString(Tag.PatientID, VR.SH, seItem.getPatientId());
		worklistItem.setString(Tag.IssuerOfPatientID, VR.LO, "LRCB");
		worklistItem.setDate(Tag.PatientBirthDate, VR.DA, DateUtil.toUtilDate(LocalDate.of(2018, 1, 1)));
		worklistItem.setString(Tag.PatientSex, VR.CS, "O");

		String aeTitle = seItem.getAeTitle();
		String seCode = seItem.getSeCode();
		String seNummer = Integer.valueOf(seCode.substring(3)).toString();
		String mammograafnummer1digit = aeTitle.substring(aeTitle.length() - 1);
		final String timestamp = seItem.getStartMoment().format(UUID_KWALITEITSOPNAME_LOCAL_DATE_TIME_FORMAT);
		worklistItem.setString(Tag.StudyInstanceUID, VR.UI, SCREENIT_BK_MAMMOGRAAF_UID + "1." + seNummer + "." + mammograafnummer1digit + "." + timestamp);

		worklistItem.setString(Tag.StudyID, VR.SH, seItem.getAccessionNumber());
		worklistItem.setString(Tag.RequestedProcedureDescription, VR.LO, "Kwaliteitsopname");
		addSequenceWithCodeValue(Tag.RequestedProcedureCodeSequence, worklistItem, risCode);

		return worklistItem;
	}

	private Attributes createWorklistItem()
	{
		Attributes worklistItem = new Attributes();

		worklistItem.setSpecificCharacterSet("ISO_IR 100");

		worklistItem.setString(Tag.RequestedProcedureID, VR.SH, "1"); 
		addReasonForRequestedProcedureCode(worklistItem);
		worklistItem.setNull(Tag.AdmissionID, VR.LO);
		worklistItem.setNull(Tag.CurrentPatientLocation, VR.LO);
		worklistItem.setNull(Tag.RequestingPhysician, VR.PN);
		return worklistItem;
	}

	private Attributes createClientMammograafWorklistItem(ClientScreenITWerklijstItem clientScreenITWerklijstItem)
	{
		String uitnodigingsNr = String.valueOf(clientScreenITWerklijstItem.getUitnodigingsNr()); 

		Attributes worklistItem = createWorklistItem();
		var risCode = clientScreenITWerklijstItem.getOnderzoekscode();
		addScheduledProcedureStep(worklistItem, clientScreenITWerklijstItem, DateUtil.toUtilDate(clientScreenITWerklijstItem.getStartDatumTijd()), "Mammography", risCode);
		worklistItem.setString(Tag.ReferringPhysicianName, VR.PN, clientScreenITWerklijstItem.getMedewerkercode());
		worklistItem.setString(Tag.RequestedProcedureDescription, VR.LO, "Mammography");
		addSequenceWithCodeValue(Tag.RequestedProcedureCodeSequence, worklistItem, risCode);
		worklistItem.setString(Tag.StudyInstanceUID, VR.UI, SCREENIT_BK_MAMMOGRAAF_UID + uitnodigingsNr); 
		worklistItem.setString(Tag.AccessionNumber, VR.SH, uitnodigingsNr);
		addPatientInfo(worklistItem, clientScreenITWerklijstItem);

		worklistItem.setString(Tag.StudyID, VR.SH, uitnodigingsNr);

		return worklistItem;
	}

	private void addScheduledProcedureStep(Attributes parent, ScreenITWerklijstItem werklijstItemData, Date date, String scheduledProcedureStepDescription,
		String scheduledProtocolCodeSequence)
	{
		Sequence sequence = parent.newSequence(Tag.ScheduledProcedureStepSequence, 9);
		String aeTitle = werklijstItemData.getAeTitle(); 
		Attributes attributes = new Attributes();
		attributes.setString(Tag.ScheduledStationAETitle, VR.AE, aeTitle);
		attributes.setDate(Tag.ScheduledProcedureStepStartDateAndTime, new DatePrecision(Calendar.SECOND), date);
		attributes.setString(Tag.Modality, VR.CS, "MG");
		attributes.setNull(Tag.ScheduledPerformingPhysicianName, VR.PN);
		attributes.setString(Tag.ScheduledProcedureStepDescription, VR.LO, scheduledProcedureStepDescription);
		attributes.setString(Tag.ScheduledStationName, VR.SH, aeTitle);
		attributes.setString(Tag.ScheduledProcedureStepLocation, VR.SH, aeTitle);
		attributes.setString(Tag.ScheduledProcedureStepID, VR.SH, "1"); 
		addSequenceWithCodeValue(Tag.ScheduledProtocolCodeSequence, attributes, scheduledProtocolCodeSequence);
		sequence.add(attributes);
	}

	private void addReasonForRequestedProcedureCode(Attributes parent)
	{
		Sequence sequence = parent.newSequence(Tag.ReasonForRequestedProcedureCodeSequence, 3);
		Attributes attributes = new Attributes();
		attributes.setNull(Tag.CodeValue, VR.SH);
		attributes.setNull(Tag.CodingSchemeDesignator, VR.SH);
		attributes.setNull(Tag.CodeMeaning, VR.LO);
		sequence.add(attributes);
	}

	private void addSequenceWithCodeValue(int tag, Attributes parent, String codeValue)
	{
		Sequence sequence = parent.newSequence(tag, 3);
		Attributes attributes = new Attributes();
		attributes.setString(Tag.CodeValue, VR.SH, codeValue);
		attributes.setNull(Tag.CodingSchemeDesignator, VR.SH);
		attributes.setNull(Tag.CodeMeaning, VR.LO);
		sequence.add(attributes);
	}

	private void addPatientInfo(Attributes worklist, ClientScreenITWerklijstItem werklijstItemData)
	{
		worklist.setString(PatientName, VR.PN, werklijstItemData.getPersonName().toString());
		worklist.setString(Tag.PatientID, VR.LO, werklijstItemData.getBsn());
		worklist.setDate(Tag.PatientBirthDate, VR.DA, DateUtil.toUtilDate(werklijstItemData.getGeboortedatum()));
		worklist.setString(Tag.IssuerOfPatientID, VR.LO, "NLMINBIZA");
		worklist.setString(Tag.PatientSex, VR.CS, bepaalGeslacht(werklijstItemData.getGeslacht()));
	}

	private String bepaalGeslacht(String geslacht)
	{
		switch (geslacht)
		{
		case "V":
			return "F";
		case "M":
			return "M";
		case "O":
			return "O";
		default:
			return null;
		}
	}
}
