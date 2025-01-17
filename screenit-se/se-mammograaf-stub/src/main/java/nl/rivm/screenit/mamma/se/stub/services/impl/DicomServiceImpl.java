package nl.rivm.screenit.mamma.se.stub.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-mammograaf-stub
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

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.concurrent.Executors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.stub.services.DicomService;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.net.ApplicationEntity;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.Connection;
import org.dcm4che3.net.DataWriterAdapter;
import org.dcm4che3.net.Device;
import org.dcm4che3.net.DimseRSPHandler;
import org.dcm4che3.net.IncompatibleConnectionException;
import org.dcm4che3.net.Priority;
import org.dcm4che3.net.Status;
import org.dcm4che3.net.pdu.AAssociateRQ;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.util.UIDUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import jakarta.annotation.PostConstruct;

@Service
@Slf4j
public class DicomServiceImpl implements DicomService
{

	@Value("${PROXY_URL}")
	private String proxyUrl;

	@Value("${AE_TITLE}")
	private String aeTitle;

	private String currentMppsUid;

	private static final String[] ONLY_DEF_TS = { UID.ImplicitVRLittleEndian };

	private final Device device = new Device("ScreenIT-SE-Device");

	private final ApplicationEntity applicationEntity = new ApplicationEntity();

	private final Connection remote = new Connection();

	private final AAssociateRQ associateRequest = new AAssociateRQ();

	private Association association;

	private Attributes result;

	@PostConstruct
	private void init()
	{
		configureExecuters();
		configureLocalConnection();
		configureRemoteConnection();
		addPresentationContexts(associateRequest);
	}

	private void configureExecuters()
	{
		var executorService = Executors.newCachedThreadPool();
		var scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();
		device.setExecutor(executorService);
		device.setScheduledExecutor(scheduledExecutorService);
	}

	private void configureLocalConnection()
	{
		var connection = new Connection();
		connection.setPort(11113);
		connection.setPackPDV(false);
		device.addConnection(connection);
		device.addApplicationEntity(applicationEntity);
		applicationEntity.addConnection(connection);
		applicationEntity.setAETitle(aeTitle); 
	}

	private void configureRemoteConnection()
	{
		associateRequest.setCalledAET("SCREENIT-SE"); 
		remote.setHostname(proxyUrl); 
		remote.setPort(11112);
		remote.setPackPDV(false);
	}

	private void addPresentationContexts(AAssociateRQ associateRequest)
	{
		associateRequest.addPresentationContext(new PresentationContext(1, UID.ModalityWorklistInformationModelFind, ONLY_DEF_TS));
		associateRequest.addPresentationContext(new PresentationContext(2, UID.ModalityPerformedProcedureStep, ONLY_DEF_TS));
		associateRequest.addPresentationContext(new PresentationContext(3, UID.MammographyCADSRStorage, ONLY_DEF_TS));

	}

	@Override
	public void queryWorklist(Attributes worklistRequest) throws IOException, InterruptedException, GeneralSecurityException, IncompatibleConnectionException
	{
		openConnection();
		result = new Attributes();
		var rspHandler = new DimseRSPHandler(association.nextMessageID())
		{
			@Override
			public void onDimseRSP(Association as, Attributes cmd, Attributes worklistItem)
			{
				super.onDimseRSP(as, cmd, worklistItem);
				var status = cmd.getInt(Tag.Status, -1);
				if (Status.isPending(status))
				{
					result = worklistItem;
				}
			}
		};
		association.cfind(UID.ModalityWorklistInformationModelFind, Priority.NORMAL, worklistRequest, null, rspHandler);
		closeConnection();
	}

	@Override
	public void sendMppsCreate(Attributes mppsMessage, boolean foutMelding) throws Exception
	{
		currentMppsUid = UIDUtils.createUID();
		openConnection();

		association.ncreate(UID.ModalityPerformedProcedureStep, currentMppsUid, mppsMessage, null, getMppsDimseRSPHandler());
		association.waitForOutstandingRSP();

		if (foutMelding)
		{
			association.ncreate(UID.ModalityPerformedProcedureStep, currentMppsUid, mppsMessage, null, getMppsDimseRSPHandler());
			association.waitForOutstandingRSP();
		}

		closeConnection();
	}

	@Override
	public void sendMppsUpdate(Attributes mppsMessage, boolean foutMelding) throws Exception
	{
		openConnection();
		association.nset(UID.ModalityPerformedProcedureStep, foutMelding ? "2.2.2.2" : currentMppsUid, mppsMessage, null, getMppsDimseRSPHandler());
		association.waitForOutstandingRSP();
		closeConnection();
	}

	private DimseRSPHandler getMppsDimseRSPHandler()
	{
		return new DimseRSPHandler(association.nextMessageID())
		{
			@Override
			public void onDimseRSP(Association association, Attributes command, Attributes data)
			{
				result = command;
				super.onDimseRSP(association, command, data);
			}
		};
	}

	private void openConnection() throws IOException, InterruptedException, IncompatibleConnectionException, GeneralSecurityException
	{
		association = applicationEntity.connect(remote, associateRequest);
	}

	private void closeConnection() throws IOException, InterruptedException
	{
		if (association != null)
		{
			association.waitForOutstandingRSP();
			association.release();
			association.waitForSocketClose();
			association = null;
		}
	}

	@Override
	public Attributes getResults()
	{
		return result;
	}

	@Override
	public String getCurrentMppsUid()
	{
		return currentMppsUid;
	}

	@Override
	public String getAETitle()
	{
		return aeTitle;
	}

	@Override
	public void sendDenseReport(Attributes denseReport) throws Exception
	{
		openConnection();
		association.cstore(UID.MammographyCADSRStorage, UIDUtils.createUID(), Priority.NORMAL,
			new DataWriterAdapter(denseReport), UID.ImplicitVRLittleEndian, getStoreDenseDimseRSPHandler());
		closeConnection();
	}

	private DimseRSPHandler getStoreDenseDimseRSPHandler()
	{
		return new DimseRSPHandler(association.nextMessageID())
		{
			@Override
			public void onDimseRSP(Association association, Attributes command, Attributes data)
			{
				result = command;
				super.onDimseRSP(association, command, data);
				if (command.getInt(Tag.Status, -1) != Status.Success)
				{
					LOG.warn("C-STORE error: {}", command);
				}
			}
		};
	}
}
