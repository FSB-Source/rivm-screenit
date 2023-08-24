package nl.rivm.screenit.mamma.se.stub.controller;

/*-
 * ========================LICENSE_START=================================
 * se-mammograaf-stub
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.security.InvalidParameterException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.mamma.se.stub.SeStubApplication;
import nl.rivm.screenit.mamma.se.stub.services.DicomService;

import org.apache.commons.lang3.time.DateUtils;
import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.DatePrecision;
import org.dcm4che3.data.PersonName;
import org.dcm4che3.data.Sequence;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.data.VR;
import org.dcm4che3.net.Status;
import org.dcm4che3.util.UIDUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class StubController
{
	private Map<String, String> viewResults;

	private Attributes lastWorklistItem;

	private SimpleDateFormat formatter;

	@Autowired
	private DicomService dicomService;

	@Value("${AE_TITLE}")
	private String aeTitle;

	public StubController()
	{
		lastWorklistItem = new Attributes();
		viewResults = new HashMap<>();

		formatter = new SimpleDateFormat();
		formatter.applyPattern("dd-MM-YYYY");
	}

	@RequestMapping("/werklijst")
	public String getWerklijst(Model model, @RequestParam(value = "foutMelding", required = false) boolean foutMelding,
		@RequestParam(value = "accessionNumber", required = false) String accessionNumber) throws Exception
	{
		Attributes worklistRequest = dicomService.loadDicomResource("mwlRequestTemplate.xml");

		Sequence scheduledProcedure = worklistRequest.getSequence(Tag.ScheduledProcedureStepSequence);
		scheduledProcedure.get(0).setDate(Tag.ScheduledProcedureStepStartDate, VR.DA, foutMelding ? DateUtils.addDays(new Date(), -1) : new Date());

		if (accessionNumber != null)
		{
			worklistRequest.setString(Tag.AccessionNumber, VR.SH, accessionNumber);
		}

		dicomService.queryWorklist(worklistRequest);

		lastWorklistItem = dicomService.getResults();

		vulWerklijstResultaten(lastWorklistItem);

		viewResults.put("DissableWorklist", "false");
		viewResults.put("AETitle", dicomService.getAETitle());
		viewResults.put("InputAccessionNumber", accessionNumber != null ? accessionNumber : lastWorklistItem.getString(Tag.AccessionNumber));

		model.addAllAttributes(viewResults);

		return "werklijst";
	}

	@RequestMapping("/mppsInProgress")
	public String sendModalityPerfomedProcedureStepInProgress(Model model, @RequestParam(value = "foutMelding", required = false) boolean foutMelding) throws Exception
	{
		Attributes mppsInProgress = dicomService.loadDicomResource("mppsInProgessTemplate.xml");

		updateMppsCreateFromWorklist(mppsInProgress);

		mppsInProgress.setDate(Tag.PerformedProcedureStepStartDateAndTime, new DatePrecision(Calendar.SECOND), new Date());

		dicomService.sendMppsCreate(mppsInProgress, foutMelding);
		vulMPPSResultaten(dicomService.getResults());

		viewResults.put("MppsUid", dicomService.getCurrentMppsUid());
		viewResults.put("SoortMpps", "Onderzoek gestart");
		viewResults.put("DissableMPPS", "false");
		viewResults.put("DissableWorklist", "true");

		model.addAllAttributes(viewResults);

		return "mppsBericht";
	}

	@RequestMapping("/mppsCompleted/{zijde}")
	public String sendModalityPerfomedProcedureStepCompleted(Model model, @PathVariable String zijde, @RequestParam(value = "foutMelding", required = false) boolean foutMelding)
		throws Exception
	{
		Attributes mppsCompleted = dicomService.loadDicomResource("mppsCompletedTemplate.xml");

		mppsCompleted.setDate(Tag.PerformedProcedureStepEndDateAndTime, new DatePrecision(Calendar.SECOND), new Date());

		switch (zijde)
		{
		case "beide":
			maakSerieFotos(mppsCompleted, "R CC");
			maakSerieFotos(mppsCompleted, "R MLO");
			maakSerieFotos(mppsCompleted, "L CC");
			maakSerieFotos(mppsCompleted, "L MLO");
			break;
		case "links":
			maakSerieFotos(mppsCompleted, "L CC");
			maakSerieFotos(mppsCompleted, "L MLO");
			break;
		case "rechts":
			maakSerieFotos(mppsCompleted, "R CC");
			maakSerieFotos(mppsCompleted, "R MLO");
			break;
		default:
			throw new InvalidParameterException("Onbekende zijde parameter binnengekomen in mppsCompleted");
		}

		dicomService.sendMppsUpdate(mppsCompleted, foutMelding);

		viewResults.put("SoortMpps", "Afgerond");
		viewResults.put("DissableMPPS", "true");
		viewResults.put("DissableWorklist", "false");

		model.addAllAttributes(viewResults);

		return "mppsBericht";
	}

	@RequestMapping("/mppsDiscontinued")
	public String sendModalityPerfomedProcedureStepDiscontinued(Model model) throws Exception
	{
		Attributes mppsDiscontinued = dicomService.loadDicomResource("mppsDiscontinuedTemplate.xml");
		mppsDiscontinued.setDate(Tag.PerformedProcedureStepEndDateAndTime, new DatePrecision(Calendar.SECOND), new Date());

		maakSerieFotos(mppsDiscontinued, "R CC");

		dicomService.sendMppsUpdate(mppsDiscontinued, false);

		viewResults.put("SoortMpps", "Afgebroken");
		viewResults.put("DissableMPPS", "true");
		viewResults.put("DissableWorklist", "false");

		model.addAllAttributes(viewResults);

		return "mppsBericht";
	}

	private void vulWerklijstResultaten(Attributes inTeVullenData)
	{
		viewResults.put("Versie", SeStubApplication.getBuildinfo().getVersion());
		viewResults.put("PatientName", inTeVullenData.getString(Tag.PatientName));
		viewResults.put("PatientSex", inTeVullenData.getString(Tag.PatientSex));
		viewResults.put("PatientID", inTeVullenData.getString(Tag.PatientID));
		viewResults.put("AccessionNumber", inTeVullenData.getString(Tag.AccessionNumber));
		viewResults.put("ReferringPhysicianName", inTeVullenData.getString(Tag.ReferringPhysicianName));

		final Sequence scheduledProcedureStepSequence = inTeVullenData.getSequence(Tag.ScheduledProcedureStepSequence);
		if (scheduledProcedureStepSequence == null)
		{
			viewResults.put("ScheduledProtocolCodeSequence", "");
		}
		else
		{
			final Sequence scheduledProtocolCodeSequence = scheduledProcedureStepSequence.get(0).getSequence(Tag.ScheduledProtocolCodeSequence);
			viewResults.put("ScheduledProtocolCodeSequence", scheduledProtocolCodeSequence != null ? scheduledProtocolCodeSequence.get(0).getString(Tag.CodeValue) : "");
		}

		final Sequence requestedProcedureCodeSequence = inTeVullenData.getSequence(Tag.RequestedProcedureCodeSequence);
		viewResults.put("RequestedProcedureCodeSequence", requestedProcedureCodeSequence != null ? requestedProcedureCodeSequence.get(0).getString(Tag.CodeValue) : "");

		Date geboortedatum = inTeVullenData.getDate(Tag.PatientBirthDate);
		viewResults.put("PatientBirthDate", geboortedatum != null ? formatter.format(geboortedatum) : "");

		viewResults.put("EmptyWorklist", String.valueOf(!lastWorklistItem.containsValue(Tag.AccessionNumber)));
	}

	private void vulMPPSResultaten(Attributes inTeVullenData)
	{
		viewResults.put("Uid", dicomService.getCurrentMppsUid());

		int status = inTeVullenData.getInt(Tag.Status, -1);
		if (status == Status.Success)
		{
			viewResults.put("Status", "Success");
		}
		else
		{
			viewResults.put("Status", "Failed");
		}

		viewResults.put("ResponseUid", inTeVullenData.getString(Tag.AffectedSOPInstanceUID));
	}

	private void updateMppsCreateFromWorklist(Attributes mppsInProgress)
	{
		mppsInProgress.setString(Tag.PatientID, VR.LO, lastWorklistItem.getString(Tag.PatientID));
		mppsInProgress.setString(Tag.PatientSex, VR.CS, lastWorklistItem.getString(Tag.PatientSex));
		mppsInProgress.setString(Tag.PatientName, VR.PN, lastWorklistItem.getString(Tag.PatientName));

		mppsInProgress.setString(Tag.StudyID, VR.SH, "1");
		mppsInProgress.setString(Tag.PatientBirthDate, VR.DA, lastWorklistItem.getString(Tag.PatientBirthDate));

		Attributes scheduledStepAttributes = new Attributes();
		Attributes worklistProcedureStep = lastWorklistItem.getSequence(Tag.ScheduledProcedureStepSequence).get(0);
		scheduledStepAttributes.setString(Tag.AccessionNumber, VR.SH, lastWorklistItem.getString(Tag.AccessionNumber));
		scheduledStepAttributes.newSequence(Tag.ReferencedStudySequence, 0);
		scheduledStepAttributes.setString(Tag.StudyInstanceUID, VR.UI, lastWorklistItem.getString(Tag.StudyInstanceUID));
		scheduledStepAttributes.setString(Tag.RequestedProcedureDescription, VR.LO, lastWorklistItem.getString(Tag.RequestedProcedureDescription));
		scheduledStepAttributes.setString(Tag.ScheduledProcedureStepDescription, VR.LO, worklistProcedureStep.getString(Tag.ScheduledProcedureStepDescription));
		scheduledStepAttributes.newSequence(Tag.ScheduledProtocolCodeSequence, 0);
		scheduledStepAttributes.setString(Tag.ScheduledProcedureStepID, VR.SH, worklistProcedureStep.getString(Tag.ScheduledProcedureStepID));
		scheduledStepAttributes.setString(Tag.RequestedProcedureID, VR.SH, lastWorklistItem.getString(Tag.RequestedProcedureID));

		mppsInProgress.newSequence(Tag.ScheduledStepAttributesSequence, 1).add(scheduledStepAttributes);
	}

	private void maakSerieFotos(Attributes attributes, String positie)
	{
		maakEnkeleFoto(attributes, UID.DigitalMammographyXRayImageStorageForProcessing, positie);
		maakEnkeleFoto(attributes, UID.DigitalMammographyXRayImageStorageForPresentation, positie);

		attributes.getSequence(Tag.ExposureDoseSequence).add(maakDoses());

		int numberOfExposures = attributes.getInt(Tag.TotalNumberOfExposures, 0) + 1;
		attributes.setInt(Tag.TotalNumberOfExposures, VR.US, numberOfExposures);
	}

	private void maakEnkeleFoto(Attributes attributes, String referencedSopClassUid, String positie)
	{
		Attributes fotoSequence = new Attributes();

		Attributes referencedImageSequence = new Attributes();
		referencedImageSequence.setString(Tag.ReferencedSOPClassUID, VR.UI, referencedSopClassUid);
		referencedImageSequence.setString(Tag.ReferencedSOPInstanceUID, VR.UI, UIDUtils.createUID());

		fotoSequence.setNull(Tag.RetrieveAETitle, VR.AE);
		fotoSequence.setString(Tag.SeriesDescription, VR.LO, positie);

		fotoSequence.setNull(Tag.PerformingPhysicianName, VR.PN);

		PersonName operatorName = new PersonName();
		operatorName.set(PersonName.Component.GivenName, "Tech");
		operatorName.set(PersonName.Component.FamilyName, "Manager");

		fotoSequence.setString(Tag.OperatorsName, VR.PN, operatorName.toString());

		fotoSequence.setString(Tag.ProtocolName, VR.LO, positie);
		fotoSequence.setString(Tag.SeriesInstanceUID, VR.UI, UIDUtils.createUID());
		fotoSequence.setNull(Tag.ReferencedNonImageCompositeSOPInstanceSequence, VR.SQ);

		fotoSequence.newSequence(Tag.ReferencedImageSequence, 2).add(referencedImageSequence);
		attributes.getSequence(Tag.PerformedSeriesSequence).add(fotoSequence);
	}

	private Attributes maakDoses()
	{
		Attributes exposureDoseSequence = new Attributes();

		exposureDoseSequence.setString(Tag.KVP, VR.DS, "28");
		exposureDoseSequence.setString(Tag.ExposureTime, VR.IS, "446");
		exposureDoseSequence.setString(Tag.RadiationMode, VR.CS, "CONTINUOUS");
		exposureDoseSequence.setString(Tag.FilterMaterial, VR.CS, "RHODIUM");
		exposureDoseSequence.setString(Tag.XRayTubeCurrentInuA, VR.DS, "1000");

		return exposureDoseSequence;
	}
}
