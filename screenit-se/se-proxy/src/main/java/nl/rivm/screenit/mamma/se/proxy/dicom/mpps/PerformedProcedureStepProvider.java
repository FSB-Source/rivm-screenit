package nl.rivm.screenit.mamma.se.proxy.dicom.mpps;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.services.MammografenStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.WerklijstStoreService;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Tag;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.Status;
import org.dcm4che3.net.service.BasicMPPSSCP;
import org.dcm4che3.net.service.DicomServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PerformedProcedureStepProvider extends BasicMPPSSCP
{
	private static final Logger LOG = LoggerFactory.getLogger(PerformedProcedureStepProvider.class);

	private static final String DISCONTINUED = "DISCONTINUED";

	private static final String COMPLETED = "COMPLETED";

	private final WerklijstStoreService werklijstStoreService;

	private final MammografenStatusService mammografenStatusService;

	public PerformedProcedureStepProvider(WerklijstStoreService werklijstStoreService, MammografenStatusService mammografenStatusService)
	{
		this.werklijstStoreService = werklijstStoreService;
		this.mammografenStatusService = mammografenStatusService;
	}

	@Override
	protected Attributes create(Association as, Attributes rq, Attributes requestData, Attributes rsp) throws DicomServiceException
	{
		var sopInstanceUid = rq.getString(Tag.AffectedSOPInstanceUID);
		var mppsRecord = createMppsRecord(requestData, sopInstanceUid);

		if (werklijstStoreService.getMppsRecord(sopInstanceUid) != null)
		{
			LOG.error("Received N-CREATE for duplicate SOP Instance UID: {}", sopInstanceUid);
			mammografenStatusService.registreerMppsFout(as, String.format("Poging om dubbel record aan te maken (accessionNr=%s)", mppsRecord.getAccessionNumber()));
			throw new DicomServiceException(Status.DuplicateSOPinstance).setUID(Tag.AffectedSOPInstanceUID, sopInstanceUid);
		}

		werklijstStoreService.addNietAfgerondeWerklijstItem(sopInstanceUid, mppsRecord);

		LOG.info("Received N-CREATE {}", mppsRecord.logTekst());
		mammografenStatusService.registreerLaatstSuccesvolleMppsBerichtVanMammograaf(as);

		return super.create(as, rq, requestData, rsp);
	}

	private MppsRecord createMppsRecord(Attributes requestData, String sopInstanceUid)
	{
		var mppsRecord = new MppsRecord(sopInstanceUid);
		mppsRecord.setAccessionNumber(requestData.getSequence(Tag.ScheduledStepAttributesSequence).get(0).getString(Tag.AccessionNumber));
		mppsRecord.setStatus(requestData.getString(Tag.PerformedProcedureStepStatus));
		return mppsRecord;
	}

	@Override
	protected Attributes set(Association as, Attributes rq, Attributes requestData, Attributes rsp) throws DicomServiceException
	{
		var sopInstanceUid = rq.getString(Tag.RequestedSOPInstanceUID);

		if (werklijstStoreService.getMppsRecord(sopInstanceUid) == null)
		{
			LOG.error("Received N-SET for not existing SOP Instance UID: {}", sopInstanceUid);
			mammografenStatusService.registreerMppsFout(as, "Update aangeroepen op een niet bestaand object (accessionNr=Onbekend)");
			throw new DicomServiceException(Status.NoSuchObjectInstance).setUID(Tag.AffectedSOPInstanceUID, sopInstanceUid);
		}

		var mppsRecord = werklijstStoreService.getMppsRecord(sopInstanceUid);
		updateMppsRecord(requestData, mppsRecord, sopInstanceUid);

		LOG.info("Received N-SET {}", mppsRecord.logTekst());
		mammografenStatusService.registreerLaatstSuccesvolleMppsBerichtVanMammograaf(as);

		return super.create(as, rq, requestData, rsp);
	}

	private void updateMppsRecord(Attributes requestData, MppsRecord mppsRecord, String sopInstanceUid)
	{

		var status = requestData.getString(Tag.PerformedProcedureStepStatus);
		mppsRecord.setStatus(status);
		if (status.equals(PerformedProcedureStepProvider.DISCONTINUED) || status.equals(PerformedProcedureStepProvider.COMPLETED))
		{
			werklijstStoreService.removeNietAfgerondeWerklijstItem(sopInstanceUid);
		}
		setDiscontinuedReasonIfExists(requestData, mppsRecord);
		setImageSidesIfExists(requestData, mppsRecord);

	}

	private void setDiscontinuedReasonIfExists(Attributes requestData, MppsRecord mppsRecord)
	{
		var reasonSequence = requestData.getSequence(Tag.PerformedProcedureStepDiscontinuationReasonCodeSequence);
		if (reasonSequence != null && !reasonSequence.isEmpty())
		{
			var code = reasonSequence.get(0).getString(Tag.CodeValue);
			var scheme = reasonSequence.get(0).getString(Tag.CodingSchemeDesignator);
			var meaning = reasonSequence.get(0).getString(Tag.CodeMeaning);
			mppsRecord.setDiscontinuedReason(String.join(" - ", code, scheme, meaning));
		}
	}

	private void setImageSidesIfExists(Attributes requestData, MppsRecord mppsRecord)
	{
		var performedSeriesSequence = requestData.getSequence(Tag.PerformedSeriesSequence);
		if (performedSeriesSequence != null && !performedSeriesSequence.isEmpty())
		{
			mppsRecord.setBeeldenRechts(false);
			mppsRecord.setBeeldenLinks(false);
			for (var attributes : performedSeriesSequence)
			{
				var protocolName = attributes.getString(Tag.ProtocolName);
				if (protocolName == null || protocolName.isEmpty())
				{
					LOG.info("Sla zijde check over voor attributes: {} ", attributes);
				}
				else
				{
					var zijde = protocolName.substring(0, 1).toUpperCase();
					switch (zijde)
					{
					case "R":
						mppsRecord.setBeeldenRechts(true);
						break;
					case "L":
						mppsRecord.setBeeldenLinks(true);
						break;
					default:
						LOG.warn("Kan geen mammogram zijde lezen uit ProtocolName: {}", protocolName);
					}
				}
			}
		}
	}
}
